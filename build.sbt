
import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers ++= {
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal)
  else
    Seq.empty[Resolver]
}

lazy val core = Project("oti-uml-canonical_xmi-serialization", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(dynamicScriptsResourceSettings(Some("org.omg.oti.uml.canonical_xmi.serialization")))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.oti,
    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,

    buildInfoPackage := "org.omg.oti.uml.canonical_xmi.serialization",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    mappings in (Compile, packageSrc) ++= {
      import Path.{flat, relativeTo}
      val base = (sourceManaged in Compile).value
      val srcs = (managedSources in Compile).value
      srcs x (relativeTo(base) | flat)
    },

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    git.baseVersion := Versions.version,

    extractArchives := {},

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    libraryDependencies ++= Seq(
      "gov.nasa.jpl.imce" %% "imce.third_party.scala_graph_libraries"
        % Versions_scala_graph_libraries.version artifacts
        Artifact("imce.third_party.scala_graph_libraries", "zip", "zip", Some("resource"), Seq(), None, Map()),

      "gov.nasa.jpl.imce" %% "imce.third_party.owlapi_libraries"
        % Versions_owlapi_libraries.version artifacts
        Artifact("imce.third_party.owlapi_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
    ),

    extractArchives := {}

  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "oti-uml-core",
    "org.omg.oti.uml.core",
    Seq(
      "org.omg.tiwg" %% "org.omg.oti.uml.core"
        % Versions_oti_uml_core.version % "compile" withSources() withJavadoc() artifacts
        Artifact("org.omg.oti.uml.core", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )


def dynamicScriptsResourceSettings(dynamicScriptsProjectName: Option[String] = None): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts/<dynamicScriptsProjectName>'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := {
      val projectName = dynamicScriptsProjectName.getOrElse(baseDirectory.value.getName)
      require(
        QUALIFIED_NAME.pattern.matcher(projectName).matches,
        s"The project name, '$projectName` is not a valid Java qualified name")
      Some(projectName)
    },

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal ++= {
      val base = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      addIfExists(base, ".classpath") ++
        (base * "*.md").pair(relativeTo(base)) ++
        addIfExists(bin, "lib/" + bin.name) ++
        addIfExists(binT, "lib/" + binT.name) ++
        addIfExists(src, "lib.sources/" + src.name) ++
        addIfExists(srcT, "lib.sources/" + srcT.name) ++
        addIfExists(doc, "lib.javadoc/" + doc.name) ++
        addIfExists(docT, "lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },

    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}