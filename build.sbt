val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    name := "mlscript-debugger-js",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    Compile / npmDependencies ++= Seq(
      "@types/vscode" -> "1.84.2",
      "@vscode/debugadapter" -> "1.64.0",
    ),
  )
