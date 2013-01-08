package chaplin

import java.io.{ IOException, Writer }
import java.util.concurrent.CountDownLatch

case class Latched(writer: Writer) extends Writer {
  
  private val latch = new CountDownLatch(1)
  private val buffer = new StringBuilder()

  @volatile var error: Option[Throwable] = None

  override def close()  {
    checkException()
    await()
    flush()
    writer.close()
  }

  override def flush() {
    checkException()
    if (latch.getCount() == 0) {
      this synchronized {
        writer.flush()
      }
    }
  }

  override def write(buf: Array[Char], off: Int, len: Int) {
    checkException()
    if (latch.getCount() == 0) writer.write(buf, off, len)
    else buffer.appendAll(buf, off, len)
  }


  def fail(t: Throwable) {
    error = Some(t)
    latch.countDown()
  }

  def complete {
    writer.append(buffer)
    latch.countDown()
  }

  def await() {
    try latch.await()
    catch { case e: InterruptedException =>
      throw new RuntimeException("Interrupted while waiting for completion", e)
    }
  }
  
  private def checkException() =
    error.map {
      case io: IOException =>
        throw new IOException(io)
    }
}
