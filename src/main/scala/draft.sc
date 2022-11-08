import java.io.File
val a = None
val b = None
val c = Some(new File(""))

val files = Seq(a,b,c)

files.flatten.map(_.getAbsoluteFile)

val dir = new File("./synthWorkspace")
val file = new File(dir, "temp.exe")

dir.isDirectory
dir.isFile
file.isDirectory
file.getAbsoluteFile
file.isHidden
file.isDirectory
file.isFile

val temp = Option(Seq(1,2,3))

temp.get.foreach(println)
