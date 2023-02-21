---
layout: post
title: Wohl, Gstreamer - How to loop videos without decoding first in Gstreamer
categories: [Programming]
---

If you've been up until it's 2am and you got a project due in 5 days ago then just jump to [here](#making-a-video-loop-without-decoding-the-video-first). Full code is available on my github

Recently I've been wrapping my head around the video library [Gstreamer](https://gstreamer.freedesktop.org/). Particularly, I've been using the Python bindings, naively believing Python would be easier than writing it in its native language, C. In retrospect, I'd perhaps recommend using C instead, it seems to be easier to follow documentation online. As an overview, my use-case is that I want to stream a video over RTSP, but require that the video loops. Surprisingly, Gstreamer has no built in functionality for this, so I dove deeper.

## The Gstreamer pipeline

Gstreamer works using a [pipeline](https://gstreamer.freedesktop.org/documentation/tutorials/basic/dynamic-pipelines.html) to link a variety of media elements. In other words, this pipeline defines where we get the data, what to do with the data and where to send the data. Let's take an example; Say we have the following pipeline:
 `filesrc location=test.mp4 ! qtdemux ! h264parse ! rtph264pay ! udpsink host=$HOST port=5000` 
 (where $HOST is localhost on my system)

This pipeline launches a RTSP Server that takes its source data from a file called test.mp4. This file is passed to a demuxer and then the raw H264 frames are parsed before being wrapped in an rtp header with rtph264pay, then your RTSP stream is accesible via UDP on $HOST at port 5000. You can try this by running this launch string with the gst-launch-1.0 command. You'll need a video file called test.mp4 of course, and some player like ffplay or vlc to connect to localhost:5000.

This is all well and good for simple RTSP serving, but what if you want to perform some processing on the frames before serving them, or wanted to serve RTSP in a programmatic way? Well thankfully gstreamer's native C-library has bindings in many languages.

### Using appsrc

Instead of using `filesrc` in the pipeline, we can define and `appsrc`. This signifies that our data is going to be coming from an [application](https://gstreamer.freedesktop.org/documentation/app/appsrc.html). You can define things like framerate and resolution in your launch string, here is an example apprsc string that takes in some data and then encodes it as h264:

{% highlight python %}
	self.launch_string = 'appsrc name=source is-live=true block=true format=GST_FORMAT_TIME ' \
                             'caps=video/x-raw,format=RGB,width={},height={},framerate={}/1 ' \
                             '! videoconvert ! video/x-raw,format=I420 ' \
                             '! x264enc speed-preset=ultrafast tune=zerolatency ' \
                             '! rtph264pay config-interval=1 name=pay0 pt=96' \
            .format(opt.image_width, opt.image_height, self.fps)
{% endhighlight %}

Notice that we pass [caps](https://gstreamer.freedesktop.org/documentation/gstreamer/gstcaps.html#GstCaps) to appsrc, including the framerate ad resolution of our data.

{% highlight python %}
    # attach the launch string to the override method
    def do_create_element(self, url):
        request_uri = url.get_request_uri()
        print('[INFO]: stream request on {}'.format(request_uri))
        self.number_frames = 0
        if not self.test:
            self.cap = get_stream_socket(self.device_id)
        return Gst.parse_launch(self.launch_string)
{% endhighlight %}

## The Gstreamer message bus

### EOS
### How EOS works

## Making a video loop without decoding the video first