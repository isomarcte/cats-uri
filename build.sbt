import cats.uri.sbt.{Versions => V}

val Scala212 = "2.12.15"
val Scala213 = "2.13.8"
val Scala3 = "3.0.2"

ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, Scala3)
ThisBuild / scalaVersion := Scala213
ThisBuild / tlBaseVersion := "0.0"

// Scalafix

ThisBuild / scalafixDependencies ++= List(
  "org.typelevel" %% "simulacrum-scalafix" % "0.5.3"
)

// Projects
lazy val root = tlCrossRootProject.aggregate(core, scalacheck, testing, benchmarks).settings(name := "cats-uri")

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "case-insensitive" % V.caseInsensitiveV,
      "org.typelevel" %%% "cats-core" % V.catsV,
      "org.typelevel" %%% "cats-parse" % V.catsParseV,
      "org.typelevel" %%% "literally" % V.literallyV
    ),
    libraryDependencies ++= {
      // Needed for macros
      if (tlIsScala3.value) {
        Nil
      } else {
        List("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
      }
    },
    console / initialCommands := {
      val wildcard: String =
        if (tlIsScala3.value) {
          "*"
        } else {
          "_"
        }
      List("cats.", "cats.syntax.all.", "cats.uri.", "cats.uri.syntax.all.")
        .map(value => s"import ${value}${wildcard}")
        .mkString("\n")
    }
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("scalacheck"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % V.scalacheckV
    ),
    console / initialCommands := {
      val wildcard: String =
        if (tlIsScala3.value) {
          "*"
        } else {
          "_"
        }
      List(
        "cats.",
        "cats.syntax.all.",
        "cats.uri.",
        "cats.uri.syntax.all.",
        "org.scalacheck.",
        "cats.uri.scalacheck.all.").map(value => s"import ${value}${wildcard}").mkString("\n")
    }
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .dependsOn(core)

lazy val testing = crossProject(JVMPlatform, JSPlatform)
  .in(file("testing"))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s"       %%% "http4s-core" % "0.23.8",
      "org.scalameta"    %%% "munit-scalacheck" % V.munitV,
      "org.typelevel"    %%% "cats-kernel-laws" % V.catsV,
      "org.typelevel"    %%% "discipline-munit" % V.disciplineMunitV
    ).map(_ % Test),
    console / initialCommands := {
      val wildcard: String =
        if (tlIsScala3.value) {
          "*"
        } else {
          "_"
        }
      List(
        "cats.",
        "cats.syntax.all.",
        "cats.uri.",
        "cats.uri.syntax.all.",
        "org.scalacheck.",
        "cats.uri.scalacheck.all.").map(value => s"import ${value}${wildcard}").mkString("\n")
    }
  ).jvmSettings(
    libraryDependencies ++= List(
      "com.google.guava" % "guava" % V.guavaV
    ).map(_ % Test)
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
  .enablePlugins(NoPublishPlugin)
  .dependsOn(scalacheck % "test -> compile")

lazy val benchmarks = project.in(file("benchmarks")).settings(
  libraryDependencies ++= List(
    "com.google.guava" % "guava" % V.guavaV,
    "org.scalacheck" %%% "scalacheck" % V.scalacheckV,
    "org.http4s"       %%% "http4s-core" % "0.23.8"
  )
).dependsOn(core.jvm).enablePlugins(NoPublishPlugin, JmhPlugin)

lazy val site = project.in(file("site")).enablePlugins(TypelevelSitePlugin).dependsOn(core.jvm)
