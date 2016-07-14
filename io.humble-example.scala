import scala.util.{Failure, Success, Try}
import java.io.{File, FileInputStream, FileOutputStream, IOException}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import io.humble.video.{Decoder, Demuxer, DemuxerStream, Global, MediaDescriptor, MediaPacket, MediaPicture, Rational}
import io.humble.video.awt.{MediaPictureConverter, MediaPictureConverterFactory}
import scala.util.control.Breaks

class IoHumbleExample{
  /**
  This function is a modified version with source from https://github.com/artclarke/humble-video/tree/master/humble-video-demos
  */
  def playVideo(videofilename:String) = {
    def writeVideoImage(videofilename:String, image:BufferedImage) = {
      try {
        val videoImage: File = new File(videofilename)
        ImageIO.write(image, "png", videoImage)
        videoImage.delete
      }
      catch {
        case ei: IOException => println(ei.getMessage)
        case x: Throwable => logger.error("Unknown exception: " + x.getMessage)
      }
    }

    /*
     * Start by creating a container object, in this case a demuxer since
     * we are reading, to get video data from.
     */
    val demuxer: Demuxer = Demuxer.make
    demuxer.open(videofilename, null, false, true, null, null) //Open the demuxer with the filename passed on.
    val numStreams: Int = demuxer.getNumStreams //Query how many streams the call to open found

    /*
    * Iterate through the streams to find the first video stream
    */
    var videoStreamId = -1
    var streamStartTime = Global.NO_PTS
    var videoDecoder: Decoder = null
    val loop = new Breaks

    loop.breakable {
      for (i <- 0 to numStreams) {
        val stream: DemuxerStream = demuxer.getStream(i)
        streamStartTime = stream.getStartTime
        val decoder: Decoder = stream.getDecoder
        if (decoder != null && decoder.getCodecType == MediaDescriptor.Type.MEDIA_VIDEO) {
          videoStreamId = i
          videoDecoder = decoder
          // stop at the first one.
          loop.break
        }
      }
    }

    if (videoStreamId == -1)
      throw new RuntimeException("could not find video stream in container: " + videofilename)

    videoDecoder.open(null, null)

    val picture: MediaPicture = MediaPicture.make(
      videoDecoder.getWidth,
      videoDecoder.getHeight,
      videoDecoder.getPixelFormat)

    val converter: MediaPictureConverter =
      MediaPictureConverterFactory.createConverter(
        MediaPictureConverterFactory.HUMBLE_BGR_24,
        picture)
    var image: BufferedImage = null

    val systemStartTime = System.nanoTime
    val systemTimeBase: Rational = Rational.make(1, 1000000000)
    val streamTimebase: Rational = videoDecoder.getTimeBase

    /**
      * Now, we start walking through the container looking at each packet. This
      * is a decoding loop, and as you work with Humble you'll write a lot
      * of these.
      *
      * Notice how in this loop we reuse all of our objects to avoid
      * reallocating them. Each call to Humble resets objects to avoid
      * unnecessary reallocation.
      */
    val packet: MediaPacket = MediaPacket.make
    var break = false
    while (demuxer.read(packet) >= 0 && !break) {
      /**
        * Now we have a packet, let's see if it belongs to our video stream
        */
      if (packet.getStreamIndex == videoStreamId) {
        /**
          * A packet can actually contain multiple sets of samples (or frames of samples
          * in decoding speak).  So, we may need to call decode  multiple
          * times at different offsets in the packet's data.  We capture that here.
          */
        var offset = 0
        var bytesRead = 0
        loop.breakable {
          while (offset < packet.getSize) {
            bytesRead += videoDecoder.decode(picture, packet, offset)
            if (picture.isComplete) {
              writeVideoImage(videofilename, displayVideoAtCorrectTime(streamStartTime, picture,
                converter, image , systemStartTime, systemTimeBase, streamTimebase))
              break = true
              offset += bytesRead
              loop.break
            }
          }
        }
      }
    }

    /** Some video decoders (especially advanced ones) will cache
     video data before they begin decoding, so when you are done you need
     to flush them. The convention to flush Encoders or Decoders in Humble Video
     is to keep passing in null until incomplete samples or packets are returned.*/
    do {
      videoDecoder.decode(picture, null, 0)
      if (picture.isComplete) {
        image = displayVideoAtCorrectTime(streamStartTime, picture, converter,
          image, systemStartTime, systemTimeBase, streamTimebase)
      }
    } while (picture.isComplete)
    demuxer.close
  }

  /**
    * Takes the video picture and displays it at the right time. This function is a modified version with source from https://github.com/artclarke/humble-video/tree/master/humble-video-demos 
    */
  def displayVideoAtCorrectTime(streamStartTime:Long, picture:MediaPicture, converter:MediaPictureConverter,
                                image:BufferedImage , systemStartTime:Long, systemTimeBase:Rational, streamTimebase:Rational):BufferedImage = {
    var streamTimestamp:Long = picture.getTimeStamp
    streamTimestamp = systemTimeBase.rescale(streamTimestamp-streamStartTime, streamTimebase)
    var systemTimestamp:Long = System.nanoTime
    while (streamTimestamp > (systemTimestamp - systemStartTime + 1000000)) {
      Thread.sleep(1)
      systemTimestamp = System.nanoTime
    }
    val videoimage = converter.toImage(image, picture)
    videoimage
  }
  
playVideo("videoname.mp4")
}
