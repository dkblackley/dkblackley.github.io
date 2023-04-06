---
layout: post
title: Zum Wohl, Gstreamer - How to loop videos without decoding first in Gstreamer
categories: [Programming]
---

If you've been up until it's 2am and you got a project due in 5 days ago then just jump to [here](#intercepting-the-signals-while-using-filesrc). Or check out the full code available on my [github](https://github.com/dkblackley/Gstreamer-example/blob/main/Server/main.py)

Recently I've been wrapping my head around the video library [Gstreamer](https://gstreamer.freedesktop.org/). Particularly, I've been using the Python bindings, naively believing Python would be easier than writing it in its native language, C. In retrospect, I'd perhaps recommend using C instead, it seems to be easier to follow documentation online. As an overview, my use-case is that I want to stream a video over RTSP, but require that the video loops. Surprisingly, Gstreamer has no built in functionality for this, so I dove deeper.

## The Gstreamer pipeline

Gstreamer works using a [pipeline](https://gstreamer.freedesktop.org/documentation/tutorials/basic/dynamic-pipelines.html) to link a variety of media elements. In other words, this pipeline defines where we get the data, what to do with the data and where to send the data. Let's take an example; Say we have the following pipeline:
 `filesrc location=test.mp4 ! qtdemux ! h264parse ! rtph264pay ! udpsink host=$HOST port=5000`
 (where $HOST is localhost on my system)

This pipeline launches a RTSP Server that takes its source data from a file called test.mp4. This file is passed to a demuxer and then the raw H264 frames are parsed before being wrapped in an rtp header with rtph264pay, then your RTSP stream is accessible via UDP on $HOST at port 5000. You can try this by running this launch string with the gst-launch-1.0 command. You'll need a video file called test.mp4 of course, and some player like ffplay or vlc to connect to localhost:5000.

This is all well and good for simple RTSP serving, but what if you want to perform some processing on the frames before serving them, or wanted to serve RTSP in a programmatic way? Well thankfully Gstreamer's native C-library has bindings in many languages.

### Using appsrc

Instead of using `filesrc` in the pipeline, we can define and `appsrc`. This signifies that our data is going to be coming from an [application](https://gstreamer.freedesktop.org/documentation/app/appsrc.html). You can define things like framerate and resolution in your launch string, here is an example apprsc string that takes in some data and then encodes it as h264:

```python
	self.launch_string = 'appsrc name=source is-live=true block=true format=GST_FORMAT_TIME ' \
                             'caps=video/x-raw,format=RGB,width={},height={},framerate={}/1 ' \
                             '! videoconvert ! video/x-raw,format=I420 ' \
                             '! x264enc speed-preset=ultrafast tune=zerolatency ' \
                             '! rtph264pay config-interval=1 name=pay0 pt=96' \
            .format(opt.image_width, opt.image_height, self.fps)
```

Notice that we pass [caps](https://gstreamer.freedesktop.org/documentation/gstreamer/gstcaps.html#GstCaps) to appsrc, which defines the frame rate and resolution of our video. Appsrc takes in single frames at a time, but we quickly run into an issue of performance, every python video library first decodes the frame before allowing the user to manipulate it (as h264 encoding relies on temporal data that would be lost when reading a single frame). We would then have to re-encode the data using ffmpeg, which takes a tremendous amount of time and gives a drastic reduction in quality (Unless you have a beefy GPU). We can easily open a file with something like OpenCv2, but how do we push this data onto the next part of the pipeline? Gstreamer is a multi-threaded library that relies on a signalling system.

## Gstreamer signals

Whenever an event occurs in Gstreamer, such as [streams requiring configuring](https://gstreamer.freedesktop.org/documentation/gst-rtsp-server/rtsp-media-factory.html?gi-language=python#GstRTSPMediaFactoryClass::media_configure), [video changing state](https://gstreamer.freedesktop.org/documentation/gstreamer/gstelement.html?gi-language=c#GstStateChange) or [clients leaving the stream](https://gstreamer.freedesktop.org/documentation/additional/design/stream-status.html?gi-language=python#messages), Gstreamer sends out a signal to notify that some work needs done. In our scenario, whenever there is space in the Gstreamer buffer for a frame, the signal "need-data" is emitted. We can connect a function to this method so push data to the buffer when needed. For example:


```python
    # attaching the source element to the rtsp media
    def do_configure(self, rtsp_media):
        appsrc = rtsp_media.get_element().get_child_by_name('source')
        appsrc.connect('need-data', self.on_need_data)
```


attaches a function called on_need_data() to the need-data signal. For completeness, here is an example on_need_data() for this application:


```python
    def on_need_data(self, src, length):

        if not self.cap.isOpened(): # Rewind video once we hit the end
            self.cap.set(cv2.CAP_PROP_POS_FRAMES, 0)

        ret, frame = self.cap.read() # read frame from opencv2
        timestamp = self.number_frames * self.duration
        frame = cv2.resize(frame, (opt.image_width, opt.image_height), \
                           interpolation=cv2.INTER_LINEAR)

        data = frame.tobytes()
        buf = Gst.Buffer.new_allocate(None, len(data), None)
        buf.fill(0, data)
        buf.duration = self.duration
        buf.pts = buf.dts = int(decoding_timestap) # Set data for RTP header
        buf.offset = timestamp
        retval = src.emit('push-buffer', buf)

        print('pushed buffer, frame {}, duration {} ns, durations {} s'.format(self.number_frames,
                                                                               self.duration,
                                                                               self.duration / Gst.SECOND))
        if retval != Gst.FlowReturn.OK:
            print(retval)
```


Of interesting note is that we emit our own signal, "push-buffer", alongside the buffer holding a frame. There is a different method that then handles this "push-buffer" signal and sends our data down the pipe. Attempting this implementation will quickly turn any laptop into a space heater, however, we can query Opencv2 to determine once we have hit the end of our video, and rewind to the beginning. Surely there must be a way to send these video frames WITHOUT doing this decoding dance? Well first lets look into some other interesting signals...

### [EOS](https://gstreamer.freedesktop.org/documentation/additional/design/events.html?gi-language=c#eos)

An EOS event is sent out by the source element once no more data is available. Usually this event is passed down the pipe to all other elements to inform them that there is no more data to be parsed. We could intercept this signal and rewind the video to the beginning by using a [SEEK](https://gstreamer.freedesktop.org/documentation/additional/design/seeking.html?gi-language=python#seeking) event with, but EOS is typically sent very late, ([and causes issues](https://stackoverflow.com/questions/53747278/seamless-video-loop-in-gstreamer)). There is, thankfully, a better signal sent out with enough time to comfortably rewind the video, the SEGMENT_DONE signal.

### [SEGMENT_DONE](https://gstreamer.freedesktop.org/documentation/additional/design/seeking.html?gi-language=c#seeking)

 SEGMENT_DONE isn't sent by default, and is triggered as part of a SEEK event with the SEGMENT flag set. As such, [we must perform an initial seek](https://stackoverflow.com/questions/53747278/seamless-video-loop-in-gstreamer) to trigger these messages on the bus. Now once we see the SEGMENT_DONE message, we can perform another SEEK event back to the beggining of the file:


```python
def seek_video(self):
    if opt.debug >= 1:
        print("Seeking...")
    self.my_player.seek(1.0,
        Gst.Format.TIME,
        Gst.SeekFlags.SEGMENT,
        Gst.SeekType.SET, 0,
        Gst.SeekType.SET, self.video_length * Gst.SECOND)

```

One important thing of note is that we do not set the flush buffer flag in our seek request. Flushing the buffer will cause some unnecessary stuttering, whereas this creates a seamless video loop.

### Intercepting the signals while using filesrc

To avoid using appsrc, we use filesrc, which removes the need for de- and re-encoding data. However, filesrc doesn't take in any data from our application, it reads it directly from the file (indeed, the string I use for a launch string could be ran with gst-launch and piped into a udpsink, removing the need for a Python file entirely!). This removes the obvious benefit that we can no longer manually count the number of frames and loop back programmatically. Thankfully, we can intercept some of these handy messages we just discussed.

When manually creating the pipeline, it's [very easy to extract the bus on which these messages are sent](https://stackoverflow.com/questions/54227361/handling-errors-with-gst-rtsp-server-python-bindings). However, Python has some very handy classes for [RTSP media factories](https://gstreamer.freedesktop.org/documentation/gst-rtsp-server/rtsp-media-factory.html?gi-language=python) and [parsing the Gstreamer launch string](https://gstreamer.freedesktop.org/documentation/gstreamer/gstparse.html?gi-language=python#gst_parse_launch) of which lazy programmers like me would like to continue using. So how do we extract the messages? We need to [create our own Gst bin](https://stackoverflow.com/questions/61604103/where-are-gstreamer-bus-log-messages) and overwrite the default message handler:


```python
def do_create_element(self, url):
    request_uri = url.get_request_uri()
    print('[INFO]: stream request on {}'.format(request_uri))

    # queue2 is better if network speed is a concern
    launch_string = "filesrc location={} ! " \
                    "qtdemux ! " \
                    "h264parse ! " \
                    "queue2 ! " \
                    "rtph264pay name=pay0 config-interval=1 pt=96".format(video_map[self.device_id])
    player = Gst.parse_launch(launch_string)

    self.video_length = int(get_length(video_map[self.device_id]))
    if int(opt.debug) >= 1:
        print("Video Length: " + str(self.video_length))

    # creates extended Gst.Bin with message debugging enabled
    self.extendedBin = ExtendedBin()
    self.extendedBin.fake_init(self.video_length, self.device_id)
    self.extendedBin.add(player)

    # creates new Pipeline and adds extended Bin to it
    self.extendedPlayer = Gst.Pipeline.new("extendedPipeline")
    self.extendedPlayer.add(self.extendedBin)
    self.extendedBin.set_player(self.extendedPlayer)

    return self.extendedPlayer

```

with our extended bin appearing as:

```python
# extended Gst.Bin that overrides do_handle_message and adds debugging
class ExtendedBin(Gst.Bin):

    def fake_init(self, length, endpoint):
        self.video_length = length - 1  # -1 for some buffer (the video pauses at the EOS
        self.endpoint = endpoint

    def set_player(self, player):
        self.my_player = player


    def do_handle_message(self, message):

        if int(opt.debug) >= 2:

            if message.type == Gst.MessageType.ERROR:
                error, debug = message.parse_error()
                print("ERROR:", message.src.get_name(), ":", error.message)
                if debug:
                    print ("Debug info: " + debug)

            elif message.type == Gst.MessageType.EOS:
                print ("End of stream")

            elif message.type == Gst.MessageType.STATE_CHANGED:
                oldState, newState, pendingState = message.parse_state_changed()
                print ("State changed -> old:{}, new:{}, pending:{}".format(oldState, newState, pendingState))

            elif message.type == Gst.MessageType.STREAM_STATUS:
                incoming, owner = message.parse_stream_status()
                print ("message: {} Owner: {}".format(incoming, owner))

            else :
                print("Some other message type: " + str(message.type))

        if message.type == Gst.MessageType.STREAM_STATUS:
            incoming, owner = message.parse_stream_status()
            if incoming == Gst.StreamStatusType.LEAVE or incoming == Gst.StreamStatusType.DESTROY:
                if int(opt.debug) >= 2:
                    print("Stream shutting down")
                # Keeping this as a separate if in case you want to do some cleanup...

        if message.type == Gst.MessageType.DURATION_CHANGED: # Called when stream has started
            print("Duration changed")
            GLib.timeout_add(25, self.seek_video) # Call the seek after the video has begun playing

        if message.type == Gst.MessageType.SEGMENT_DONE:
            self.seek_video()

        Gst.Bin.do_handle_message(self, message)
```

## Some Closing notes

In retrospect, starting with the C bindings would likely leave you with a lot less problems, due to the abundance of material online to help newcomers. Perhaps I'll try re-writing this in C to get more comfortable with those bindings. As another aside, there are other ways to achieve what I have here with filesrc. One way I experimented with was stripping the mp4 container from the file and simply [trying to use the NAL packet headers to extract h264 chunks](https://stackoverflow.com/questions/1685494/what-does-this-h264-nal-header-mean). You could then use appsrc and pass these into a h264 parser, but debugging the parsing of binary numbers comes with it's own headaches. All in all, this method works well for my use case, and hopefully you've learnt enough along the way to make a Gstreamer application to you use case.

