/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.canonicalXMI

import org.omg.oti.uml.UMLError

import java.io.{File, FileNotFoundException, IOException}
import java.lang.{Exception, IllegalArgumentException, Throwable}
import java.net.{MalformedURLException, URI, URL}

import org.omg.oti.uml.read.api.UML

import scala.{Boolean, Int, Option, None, Some, StringContext, Unit}
import scala.Predef.{require, String}
import scala.collection.immutable._

import scalaz._, Scalaz._, syntax.std._

import org.apache.xml.resolver.Catalog
import org.apache.xml.resolver.CatalogManager
import org.apache.xml.resolver.tools.CatalogResolver

class CatalogURIMapperException
(override val message: String,
 override val cause: Option[Throwable] = None)
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
  : ValidationNel[UMLError.UException, Unit] =
    scala.util.Try(catalog.parseCatalog(catalogURI.toURL))
      .cata(
        success = (s: Unit) => s.successNel[UMLError.UException],
        failure = (t: java.lang.Throwable) => catalogURIMapperException(s"failed to parse catalog: $catalogURI", Some(t)).failureNel
      )

  def loadResolutionStrategy
  (appendDocumentExtensionUnlessPresent: Option[String])
  (resolved: String)
  : ValidationNel[UMLError.UException, Option[URI]] =

    if (!appendDocumentExtensionUnlessPresent.getOrElse(".").startsWith("."))
      catalogURIMapperException(
        "The document extension, when specified, must start with '.'",
        new IllegalArgumentException(
          s"Illegal value for appendDocumentExtensionUnlessPresent: " +
            s"'$appendDocumentExtensionUnlessPresent'").some).failureNel

    else

      scala.util.Try({
        val normalized = new URI(resolved)
        val normalizedPath = normalized.toString

        val f1 = new URL(normalizedPath)
        val f2 = appendDocumentExtensionUnlessPresent.fold[URL](f1) { ext =>
          if ( normalizedPath.endsWith( ext ) )
            f1
          else
            new URL( normalizedPath + ext )
        }
        (f1, f2)
      })
        .cata(
          success = (f12: (URL, URL))=> loadResolutionStrategyPair(f12._1, f12._2),
          failure = (t: java.lang.Throwable) => catalogURIMapperException(s"failed to parse '$resolved' as a URL", Some(t)).failureNel
        )

  def loadResolutionStrategyPair
  (f1: URL, f2: URL)
  : ValidationNel[UMLError.UException, Option[URI]] =
    scala.util.Try(
      for {
        is <- Option.apply(f2.openStream)
        if is.available() > 0
      } yield {
        is.close()
        f2.toURI
      })
      .cata(
        success = (r: Option[URI]) => r.successNel,
        failure = (_: java.lang.Throwable) =>
          scala.util.Try(
            for {
              is <- Option.apply(f1.openStream())
              if is.available() > 0
            } yield {
              is.close()
              f1.toURI
            })
            .cata(
              success = (r: Option[URI]) => r.successNel,
              failure = (_: java.lang.Throwable) => Option.empty[URI].successNel[UMLError.UException])
      )

  def saveResolutionStrategy(resolved: String): Option[URI] = {
    val normalized = new URI(resolved)
    val normalizedPath = normalized.toString
    val f1 = new URL(normalizedPath)
    val outputFile =
      if (resolved.startsWith("file:")) new File(resolved.substring(5))
      else new File(resolved)
    outputFile.getParentFile match {
      case null => None
      case outputDir =>
        if (!outputDir.exists)
          outputDir.mkdirs

        if (outputDir.exists && outputDir.isDirectory && outputDir.canWrite)
          Some(f1.toURI)
        else
          None
    }
  }

  def resolve(uri: String): Option[String] =
    catalog.resolveURI(uri) match {
      case null => None
      case resolved => Some(resolved)
    }

  def resolveURI
  ( uri: URI,
    resolutionStrategy: (String) => Option[URI])
  : ValidationNel[UMLError.UException, Option[URI]] = {

    val rawPath = uri.toString
    val iriPath =
      if (rawPath.endsWith("#"))
        rawPath.substring(0, rawPath.length() - 1)
      else
        rawPath
    try {
      Option.apply(resolve(iriPath))
      .fold[ValidationNel[UMLError.UException, Option[URI]]](None.successNel) {
      resolved =>
        resolved.fold[ValidationNel[UMLError.UException, Option[URI]]](None.successNel) { r =>
          resolutionStrategy(r).successNel
        }
      }
    }
    catch {
      case t: MalformedURLException =>
        catalogURIMapperException(s"resolveURI(uri=$uri) failed", t.some).failureNel
      case t: IOException =>
        catalogURIMapperException(s"resolveURI(uri=$uri) failed", t.some).failureNel
    }
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
  ( catalogFiles: Seq[File],
    verbosity: Int = 0)
  : ValidationNel[UMLError.UException, CatalogURIMapper] = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: ValidationNel[UMLError.UException, CatalogURIMapper] = mapper.successNel
    val cN: ValidationNel[UMLError.UException, CatalogURIMapper] =
      ( c0 /: catalogFiles) { ( ci, catalogFile ) =>
      if (!catalogFile.exists)
        ci +++ catalogURIMapperException(
          s"createMapperFromCatalogFiles failed",
          new FileNotFoundException(catalogFile.getAbsolutePath).some).failureNel
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
  : ValidationNel[UMLError.UException, CatalogURIMapper] = {
    val catalog = new CatalogManager()
    catalog.setUseStaticCatalog(false)
    catalog.setRelativeCatalogs(true)
    catalog.setVerbosity(verbosity)
    val mapper = new CatalogURIMapper(catalog)
    val c0: ValidationNel[UMLError.UException, CatalogURIMapper] = mapper.successNel
    val cN: ValidationNel[UMLError.UException, CatalogURIMapper] =
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