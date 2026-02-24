organization := "io.hookline"
name         := "hookline-sdk"
version      := "0.2.0"
scalaVersion := "3.3.4"
crossScalaVersions := Seq("3.3.4", "3.4.2")

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % "2.1.9",
  "dev.zio" %% "zio-http"     % "3.0.1",
  "dev.zio" %% "zio-json"     % "0.7.3",
  "dev.zio" %% "zio-test"     % "2.1.9" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.1.9" % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

// GitHub Packages publishing
publishTo := Some(
  "GitHub Packages" at
    "https://maven.pkg.github.com/saidmashhud/hookline"
)

credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_ACTOR", ""),
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

publishMavenStyle      := true
Test / publishArtifact := false

pomExtra :=
  <url>https://github.com/saidmashhud/hookline</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>https://opensource.org/licenses/MIT</url>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/saidmashhud/hookline</url>
    <connection>scm:git:git://github.com/saidmashhud/hookline.git</connection>
  </scm>
  <developers>
    <developer>
      <id>saidmashhud</id>
      <name>Said Mashhud</name>
    </developer>
  </developers>
