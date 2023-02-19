---
layout: post
title: Wohl, Gstreamer
categories: [Programming]
---

Recently I've been wrapping my head around the video library [Gstreamer](https://gstreamer.freedesktop.org/). Particularly, I've been using the Python bindings, naively believing Python would be easier than writing it in its native language, C. In retrospect, I'd perhaps recommend using C instead, it seems to be easier to follow documentation online. As an overview, my use-case is that I want to stream a video over RTSP, but require that the video loops. Surprisingly, Gstreamer has no built in functionality for this, so I dove deeper.

## The Gstreamer pipeline

Gstreamer works using a [pipeline](https://gstreamer.freedesktop.org/documentation/tutorials/basic/dynamic-pipelines.html) to link a variety of media elements. In other words, this pipeline defines where we get the data, what to do with the data and where to send the data. Let's take an example; Say we have the following pipeline:
 `filesrc location=test.mp4 ! qtdemux ! h264parse ! rtph264pay ! udpsink host=$HOST port=5000` 
 (where $HOST is localhost on my system)

This pipeline launches a RTSP Server that streams a file called test.mp4. This file is passed to a demuxer and then the raw H264 frames are parsed before being wrapped in an rtp header with rtph264pay, then you RTSP stream is accesible via UDP on $HOST at port 5000. You can try this by running this launch string with the gst-launch-1.0 command. You'll need a video file called test.mp4 of course.

This is all well and good for simple rtsp serving, but what if you want to perform some processing on the frames before serving them, or wanted to serve rtsp in a programmatic way? Well thankfully gstreamer's native C-library has bindings in many languages.

### Using appsrc

Instead of using `filesrc` in the pipeline, we can define and `appsrc`, which we can connect a function to call whenever gstreamer needs some new data to throw down the pipe: