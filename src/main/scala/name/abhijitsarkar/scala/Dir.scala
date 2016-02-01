package name.abhijitsarkar.scala

import java.nio.file.{DirectoryStream, Files, Path, Paths}

import scala.collection.JavaConversions._
import scala.util._

/**
  * @author Abhijit Sarkar
  */
/**
  * Q8.3: Let’s create a directory listing class.
  * The constructor fields should be the full path to the directory
  * and a predicate function that takes a String (the filename)
  * and returns true if the file should be included.
  * The method `list` should then list the files in the directory.
  * - Is there any part of this class that would work well as a lazy value?
  * - Would it make sense to store the anonymous subclass of `java.io.FilenameFilter` as a `lazy val`?
  * - How about the filtered directory listing?
  *
  * Ans: The original question asked to use `java.io.File.listFiles(filter: FilenameFilter)` but because
  * `java.nio` effectively replaced most classes of `java.io`, I used a `DirectoryStream` and a
  * `DirectoryStream.Filter` instead. Only caveat is that Scala does not yet support Java 8 functional interfaces
  * and streams (this will change soon) , so I had to implement `DirectoryStream.Filter` as an anonymous class
  * instead of a lambda expression.
  *
  * As for the follow up questions:
  * - The filter works well as a lazy value because if method `list` is never invoked, no instances
  * of `DirectoryStream.Filter` is created.
  * - Yes, noting that I used a `DirectoryStream.Filter` and not a `java.io.FilenameFilter`.
  * - I do not see any benefit in storing the filtered directory listing as a `lazy val`.
  */
class Dir(root: String, p: String => Boolean) {
  val absolutePath = (p: Path) => p.toFile.getAbsolutePath
  lazy val filter = new DirectoryStream.Filter[Path]() {
    override def accept(path: Path): Boolean = p(absolutePath(path))
  }

  def list: List[String] = {
    var ds: DirectoryStream[Path] = null

    val t = Try {
      ds = Files.newDirectoryStream(Paths.get(root), filter)
      val l = ds.iterator().map(absolutePath).toList
      ds.close()

      l
    }

    t match {
      case Success(s) => s
      case Failure(ex) => {
        ex.printStackTrace;
        if (ds != null) ds.close

        Nil
      }
    }
  }
}
