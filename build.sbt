val scala3Version = "3.3.1"

Global / excludeLintKeys += webpackExtraArgs

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    name         := "mlscript-debugger-js",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,

    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2",
    libraryDependencies += "co.fs2" %%% "fs2-core" % "3.9.3",
    libraryDependencies += "co.fs2" %%% "fs2-io" % "3.9.3",
    
    webpack / version := "5.89.0",
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    },
    Compile / fastOptJS / webpackExtraArgs ++= Seq(
      "--output-hash-function=xxhash64",
      "--externals=vscode",
      "--externals-type=commonjs",
      "--target=node",
      "--output-library-type=commonjs2",
    ),
    Compile / npmDependencies ++= Seq(
      "@types/vscode"        -> "1.84.2",
      "@vscode/debugadapter" -> "1.64.0"
    )
  )
