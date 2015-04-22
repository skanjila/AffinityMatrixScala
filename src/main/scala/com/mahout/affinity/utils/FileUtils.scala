package com.mahout.affinity.utils


import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.FileReader
import java.io.FileWriter
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.PrintWriter
import java.net.URL
import java.net.URLConnection
import java.nio.channels.FileChannel
import java.util.ArrayList
import java.util.Collections
import java.util.List
import java.util.logging.Level
import java.util.logging.Logger
import java.util.regex.Pattern
import scala.collection.JavaConversions._

object FileUtils {

  val LOG = Logger.getLogger(FileUtils.getClass.getName)

  def temporaryDirectory(): String = {
    val tempDir = System.getProperty("java.io.tmpdir")
    if (tempDir.contains(" ")) {
      LOG.warning("Temporary directory (" + tempDir + ") contains whitespace")
    }
    tempDir
  }

  def combine(path: String*): String = {
    var file = new File(path(0))
    for (i <- 1 until path.length) {
      file = new File(file, path(i))
    }
    file.getPath
  }

  def runtimeDirectory(): String = {
    var runtimePath = ""
    try {
      runtimePath = new File(".").getCanonicalPath
    } catch {
      case e: IOException => LOG.log(Level.SEVERE, "Could not find runtime path, strange.", e)
    }
    runtimePath
  }

  def writeFile(contents: String, name: String) {
    writeFile(contents, name, false)
  }

  private def writeFile(contents: String, name: String, append: Boolean) {
    var outputStream: BufferedWriter = null
    var output: PrintWriter = null
    var fileWriter: FileWriter = null
    try {
      fileWriter = new FileWriter(name, append)
      outputStream = new BufferedWriter(fileWriter)
      output = new PrintWriter(outputStream)
      output.print(contents)
      outputStream.flush()
      output.close()
    } catch {
      case e: IOException => LOG.log(Level.SEVERE, "Could not write file " + name, e)
    } finally {
      try {
        if (outputStream != null) {
          outputStream.close()
        }
        if (output != null) {
          output.close()
        }
      } catch {
        case e: IOException => LOG.log(Level.SEVERE, "Failed to close file: " + name, e)
      }
    }
  }

  def appendFile(contents: String, name: String) {
    writeFile(contents, name, true)
  }

  def readFile(name: String): String = {
    var fileReader: FileReader = null
    val contents = new StringBuilder()
    try {
      val file = new File(name)
      if (!file.exists()) {
        throw new IllegalArgumentException("File " + name + " does not exist")
      }
      fileReader = new FileReader(file)
      val reader = new BufferedReader(fileReader)
      var inputLine = reader.readLine()
      while (inputLine != null) {
        contents.append(inputLine).append("\n")
        inputLine = reader.readLine()
      }
      reader.close()
    } catch {
      case i1: IOException => LOG.severe("Can't open file:" + name)
    }
    contents.toString
  }


  def readCSVFile(fileName: String, separator: String, expectedColumns: Int): List[Array[String]] = {
    val data = new ArrayList[Array[String]]()
    var fileReader: FileReader = null
    var in: BufferedReader = null
    try {
      val file = new File(fileName)
      if (!file.exists()) {
        throw new IllegalArgumentException("File '" + fileName + "' does not exist")
      }
      fileReader = new FileReader(file)
      in = new BufferedReader(fileReader)
      var inputLine: String = null
      var lineNumber = 0
      inputLine = in.readLine()
      while (inputLine != null) {
        lineNumber += 1
        val row = inputLine.split(separator)
        if (expectedColumns == -1 || expectedColumns == row.length) {
          data.add(row)
        } else {
          throw new AssertionError("Unexpected row length (line " + lineNumber + " ). " + 
            "Expected:" + 
            expectedColumns + 
            " real " + 
            row.length + 
            ". CVS-file incorrectly formatted?")
        }
        inputLine = in.readLine()
      }
    } catch {
      case i1: IOException => LOG.severe("Can't open file:" + fileName)
    } finally {
      if (in != null) {
        try {
          in.close()
        } catch {
          case e: IOException => 
        }
      }
    }
    data
  }

  trait RowFilter {

    def acceptRow(row: Array[String]): Boolean
  }

  val ACCEPT_ALL_ROWFILTER = new RowFilter() {

    def acceptRow(row: Array[String]): Boolean = true
  }

  def readColumnFromCSVData(data: List[Array[String]], columnIndex: Int, filter: RowFilter): List[String] = {
    val actualFilter = if (filter == null) ACCEPT_ALL_ROWFILTER else filter
    val columnData = new ArrayList[String]()
    for (row <- data if actualFilter.acceptRow(row)) {
      columnData.add(row(columnIndex))
    }
    columnData
  }

  def glob(directory: String, pattern: String, recursive: Boolean): List[String] = {
    val matchingFiles = new ArrayList[String]()
    val p = Pattern.compile(pattern)
    val dir = new File(new File(directory).getAbsolutePath)
    glob(dir, p, recursive, matchingFiles)
    Collections.sort(matchingFiles)
    matchingFiles
  }

  private def glob(directory: File, 
      pattern: Pattern, 
      recursive: Boolean, 
      matchingFiles: List[String]) {
    if (!directory.isDirectory) {
      throw new IllegalArgumentException(directory + " is not a directory")
    }
    for (file <- directory.list()) {
      val filePath = new File(FileUtils.combine(directory.getAbsolutePath, file))
      if (recursive && filePath.isDirectory) {
        glob(filePath, pattern, recursive, matchingFiles)
      } else {
        if (pattern.matcher(file).matches() && file != null) {
          matchingFiles.add(filePath.getAbsolutePath)
        }
      }
    }
  }

  def extension(fileName: String): String = {
    val dot = fileName.lastIndexOf('.')
    if (dot == -1) "" else fileName.substring(dot + 1)
  }

  def basename(fileName: String): String = {
    var dot = fileName.lastIndexOf('.')
    var sep = fileName.lastIndexOf(File.separatorChar)
    if (sep == -1) {
      sep = fileName.lastIndexOf('\\')
    }
    if (dot == -1) {
      dot = fileName.length
    }
    fileName.substring(sep + 1, dot)
  }

  def path(fileName: String): String = {
    val sep = fileName.lastIndexOf(File.separatorChar)
    fileName.substring(0, sep)
  }

  def exists(fileName: String): Boolean = new File(fileName).exists()

  def mkdirs(path: String): Boolean = new File(path).mkdirs()

  def cp(source: String, target: String) {
    var inChannel: FileChannel = null
    var outChannel: FileChannel = null
    try {
      inChannel = new FileInputStream(new File(source)).getChannel
      outChannel = new FileOutputStream(new File(target)).getChannel
      inChannel.transferTo(0, inChannel.size, outChannel)
    } catch {
      case e: FileNotFoundException => LOG.severe("File " + source + " not found! " + e.getMessage)
      case e: IOException => LOG.severe("Error while copying " + source + " to " + target + " : " + 
        e.getMessage)
    } finally {
      try {
        if (inChannel != null) {
          inChannel.close()
        }
        if (outChannel != null) {
          outChannel.close()
        }
      } catch {
        case e: IOException => LOG.log(Level.INFO, "Ignored exception while copying files", e)
      }
    }
  }

  def rm(fileName: String): Boolean = new File(fileName).delete()

  def isDirectory(inputFile: String): Boolean = new File(inputFile).isDirectory
}
