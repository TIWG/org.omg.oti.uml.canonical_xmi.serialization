/*
 * Copyright 2014 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Copyright 2015 Airbus.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package org.omg.oti.uml.canonicalXMI

import org.omg.oti.uml.UMLError

import java.net.{URI, URL}

import scala.{Int, Option, StringContext, Unit}
import scala.Predef.{classOf, String}
import scala.collection.immutable._
import scala.util.control.Exception._

import scalaz._, Scalaz._, syntax.std._

import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.apache.xml.resolver.tools.CatalogResolver

class CatalogURIMapperException
(override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause)

case class CatalogURIMapper(
                             catalogManager: CatalogManager,
                             catalogResolver: CatalogResolver,
                             catalog: Catalog) {

  def this(catalogManager: CatalogManager,
           catalogResolver: CatalogResolver) =
    this(catalogManager, catalogResolver, catalogResolver.getCatalog)

  def this(catalogManager: CatalogManager) =
    this(catalogManager, new CatalogResolver(catalogManager))

  def parseCatalog(catalogURI: URI)
  : Set[java.lang.Throwable] \/ Unit =
    catching(classOf[java.io.IOException])
    .withApply{ cause: java.lang.Throwable =>
      Set(catalogURIMapperException(s"failed to parse catalog: $catalogURI", cause)).left
    }
    .apply(catalog.parseCatalog(catalogURI.toURL).right)

  def loadResolutionStrategy
  (appendDocumentExtensionUnlessPresent: Option[String])
  (resolved: String)
  : Set[java.lang.Throwable] \/ Option[URI] =

    if (!appendDocumentExtensionUnlessPresent.getOrElse(".").startsWith("."))
      Set(
        catalogURIMapperException(
          "The document extension, when specified, must start with '.'",
          new java.lang.IllegalArgumentException(
            s"Illegal value for appendDocumentExtensionUnlessPresent: " +
              s"'$appendDocumentExtensionUnlessPresent'"))).left

    else
      nonFatalCatch
      .withApply { cause: java.lang.Throwable =>
        Set(
          catalogURIMapperException(s"failed to parse '$resolved' as a URL", cause)).left
      }
      .apply({
        val normalized = new URI(resolved)
        val normalizedPath = normalized.toString

        val f1 = new URL(normalizedPath)
        val f2 = appendDocumentExtensionUnlessPresent.fold[URL](f1) { ext =>
          if ( normalizedPath.endsWith( ext ) )
            f1
          else
            new URL( normalizedPath + ext )
        }
        loadResolutionStrategyPair(f1, f2)
      })

  def loadResolutionStrategyPair
  (f1: URL, f2: URL)
  : Set[java.lang.Throwable] \/ Option[URI] =
    catching(
      classOf[java.io.IOException],
      classOf[java.lang.SecurityException],
      classOf[java.net.URISyntaxException])
    .withApply { _: java.lang.Throwable =>
      Option.empty[URI].right
    }
    .apply({
      val r1: Option[URI] =
        for {
          is <- Option.apply(f2.openStream)
          if is.available() > 0
        } yield {
          is.close()
          f2.toURI
        }

      r1
      .fold[Set[java.lang.Throwable] \/ Option[URI]](
          catching(
            classOf[java.io.IOException],
            classOf[java.lang.SecurityException])
          .withApply { _: java.lang.Throwable =>
            Option.empty[URI].right
          }
          .apply({
            val r2: Option[URI] = for {
              is <- Option.apply(
                f1.openStream()
              )
              if is.available() > 0
            } yield {
              is.close()
              f1.toURI
            }
            r2
              .fold[Set[java.lang.Throwable] \/ Option[URI]](
              Option.empty[URI].right
            ) { uri: URI =>
              uri.some.right
            }
          })
        ) { uri: URI =>
          uri.some.right
        }
    })

  def saveResolutionStrategy(resolved: String)
  : Set[java.lang.Throwable] \/ Option[URI] =
  nonFatalCatch
  .withApply {
    (cause: java.lang.Throwable) =>
      Set(
        catalogURIMapperException(
          s"saveResolutionStrategy: resolved=$resolved failed: ${cause.getMessage}",
          cause)
      ).left
  }
  .apply({
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val f1 = new URL(normalizedPath)
    val outputFile =
      if (resolved.startsWith("file:"))
        new java.io.File(resolved.substring(5))
      else
        new java.io.File(resolved)
    Option.apply(outputFile.getParentFile)
    .fold[Option[URI]](Option.empty[URI]) { outputDir =>
      if (!outputDir.exists) {
        outputDir.mkdirs
      }
      if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
        f1.toURI.some
      else
        Option.empty[URI]
    }
    .right
  })

  def resolve(uri: String)
  : Set[java.lang.Throwable] \/ Option[String] =
    catching(classOf[java.net.MalformedURLException])
    .withApply { cause: java.lang.Throwable =>
      Set(
        catalogURIMapperException(s"resolve(uri=$uri) failed: ${cause.getMessage}", cause)
      ).left
    }
    .apply(
      Option.apply(
        catalog.resolveURI(uri)
      ).right
    )

  def resolveURI
  ( uri: URI,
    resolutionStrategy: (String) => Set[java.lang.Throwable] \/ Option[URI])
  : Set[java.lang.Throwable] \/ Option[URI] = {

    val rawPath = uri.toString
    val iriPath =
      if (rawPath.endsWith("#"))
        rawPath.substring(0, rawPath.length() - 1)
      else
        rawPath
    catching(
      classOf[java.net.MalformedURLException],
      classOf[java.io.IOException])
    .withApply { t: java.lang.Throwable =>
      Set(
        catalogURIMapperException(s"resolveURI(uri=$uri) failed", t)
      ).left
    }
    .apply(
      resolve(iriPath)
      .flatMap {
        _.fold[Set[java.lang.Throwable] \/ Option[URI]](
          Option.empty[URI].right
        ) { resolved =>
          resolutionStrategy(resolved)
        }
      })
  }
}

object CatalogURIMapper {

  type CatalogURIMapperAdd = (CatalogURIMapper, => CatalogURIMapper) => CatalogURIMapper

  /**
   * Creates a CatalogURIMapper from Catalog files.
   *
   * @param catalogFiles
   * @param verbosity
   * @return
   */
  def createMapperFromCatalogFiles
  ( catalogFiles: Seq[java.io.File],
    verbosity: Int = 0)
  : Set[java.lang.Throwable] \/ CatalogURIMapper = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: Set[java.lang.Throwable] \/ CatalogURIMapper = mapper.right
    val cN: Set[java.lang.Throwable] \/ CatalogURIMapper =
      ( c0 /: catalogFiles) { ( ci, catalogFile ) =>
      if (!catalogFile.exists)
        ci +++
          Set(
            catalogURIMapperException(
              s"createMapperFromCatalogFiles failed",
              new java.io.FileNotFoundException(catalogFile.getAbsolutePath))).left
      else
        ci +++ mapper.parseCatalog(catalogFile.toURI).map( _ => mapper)
    }

    cN
  }

  /**
   * Creates a CatalogURIMapper from Catalog file URIs
   *
   * @param catalogURIs URIs of the Catalog files
   * @param verbosity
   * @return
   */
  def createMapperFromCatalogURIs
  ( catalogURIs: Seq[URI],
    verbosity: Int = 0)
  : Set[java.lang.Throwable] \/ CatalogURIMapper = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: Set[java.lang.Throwable] \/ CatalogURIMapper = mapper.right
    val cN: Set[java.lang.Throwable] \/ CatalogURIMapper =
      ( c0 /: catalogURIs) { ( ci, catalogURI ) =>
      ci +++ mapper.parseCatalog(catalogURI).map( _ => mapper)
    }

    cN
  }


  // @todo is this a semigroup? if not, then what's a better way of achieving the desired result?

  implicit def CatalogURIMapperSemigroup: Semigroup[CatalogURIMapper] =
    new Semigroup[CatalogURIMapper] {
      def append(x: CatalogURIMapper, y: => CatalogURIMapper): CatalogURIMapper = x
    }

}