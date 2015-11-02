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

import java.lang.Integer
import java.net.URL

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.OTISpecificationRootCharacteristics
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.xmi._

import scala.{Boolean,Int,Option,None,Some,StringContext,Unit}
import scala.Predef.{require,String}
import scala.collection.immutable._
import scala.Predef.{Set => _, Map => _, _}
import scala.language.postfixOps
import scala.util.control.Exception._
import scalaz._, Scalaz._


class DocumentIDGeneratorException[Uml <: UML]
(idGenerator: DocumentIDGenerator[Uml],
 override val element: Iterable[UMLElement[Uml]],
 override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UElementException[Uml, UMLElement[Uml]](element, message, cause)

/**
 * @tparam Uml
 */
trait DocumentIDGenerator[Uml <: UML] extends IDGenerator[Uml] {

  import umlOps._

  implicit val documentOps: DocumentOps[Uml]

  implicit val resolvedDocumentSet: ResolvedDocumentSet[Uml]

  protected val element2id: Element2IDHashMap

  protected val elementRules: List[Element2IDRule]

  protected val containmentRules: List[ContainedElement2IDRule]

  override def element2mappedDocument
  (e: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Option[Document[Uml]] =
    resolvedDocumentSet.element2mappedDocument(e).right

  override def getElement2IDMap
  : Map[UMLElement[Uml], (NonEmptyList[java.lang.Throwable] \/ String)] = element2id.toMap

  override def lookupElementXMI_ID
  (e: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Option[String] =
    element2id
    .get(e)
    .fold[NonEmptyList[java.lang.Throwable] \/ Option[String]](
      Option.empty[String].right
    ){ id =>
      id.map(_.some)
    }

  /**
   * Computes the xmi:ID for each element in the domain of the element2document map of the ResolvedDocumentSet
   */
  def computePackageExtentXMI_ID(pkg: UMLPackage[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Unit = {

    val extent = pkg
      .allOwnedElements
      .+(pkg)
      .filter(resolvedDocumentSet.isElementMapped2Document)

    val e0: NonEmptyList[java.lang.Throwable] \/ Unit = ().right
    val eN: NonEmptyList[java.lang.Throwable] \/ Unit = (e0 /: extent) {
      (ei, e) =>
        ei +++ getXMI_ID(e).map( _ => ())
    }

    eN
  }

  protected def getXMI_IDREF_or_HREF_fragment
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ String =
    getXMI_IDREF_or_HREF_fragment_internal(from, to)
    .orElse(
      getMappedOrReferencedElement(to)
        .flatMap { _to =>
          getXMI_IDREF_or_HREF_fragment(from, _to)
        })

  protected def getXMI_IDREF_or_HREF_fragment_internal
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ String =
    resolvedDocumentSet.element2mappedDocument(from)
    .fold[NonEmptyList[java.lang.Throwable] \/ String] {
      NonEmptyList(
        UMLError
        .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference from", Iterable(from)))
      .left
    }{ d1 =>
      resolvedDocumentSet.element2mappedDocument(to)
      .fold[NonEmptyList[java.lang.Throwable] \/ String] {
        NonEmptyList(
          UMLError
          .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference to", Iterable(to)))
        .left
      }{
        case db2: BuiltInDocument[Uml] =>
          require(d1 != db2)
          // Based on the built-in 'to' element ID, construct the built-in URI for the 'to' element.
          val bid = for {
            builtIn_d2_id <- to.toolSpecific_id.fold[NonEmptyList[java.lang.Throwable] \/ String] {
              -\/(
                NonEmptyList(
                  documentIDGeneratorException(
                    this,
                    Iterable(from, to),
                    s"getXMI_IDREF_or_HREF_fragment_internal: error: There should be a tool-specific xmi:id for the 'to' element in $db2")))
            }{ id =>
              \/-(id)
            }

            builtInURI <- documentOps.getExternalDocumentURL(db2.documentURL)

            builtInURITo =
              builtInURI
              .resolve("#" + builtIn_d2_id)
              .toString

            // use the builtInURIMapper to convert the built-in URI of the 'to' element into an OMG URI
            mappedURITo <- resolvedDocumentSet.ds.builtInURIMapper.resolve(builtInURITo).map(_.getOrElse(builtInURITo))
            fragmentIndex = mappedURITo.lastIndexOf('#')
            _ = require(fragmentIndex > 0)

            // It's not needed to add the prefix since it's already included in the computed ID
            fragment = IDGenerator.xmlSafeID(mappedURITo.substring(fragmentIndex + 1))
          } yield IDGenerator.xmlSafeID(fragment)

          bid

        case _: SerializableDocument[Uml] =>
          getMappedOrReferencedElement(to)
          .flatMap( _to => getXMI_ID(_to))

        case d =>
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(from, to),
              s"getXMI_IDREF_or_HREF_fragment_internal: error: Unknown document $d for element reference to"))
          .left
      }
    }

  /**
   * The xmi:ID of an element depends on what kind of document it is contained in.
   * - BuiltInDocument: this is deferred to builtInID, which is implementation-specific.
   * - SerializableDocument: this is the OTI implementation of Canonical XMI ID
   * unless it is overriden by an application of the OTI::Identity stereotype
   */
  def getXMI_ID(self: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ String =
    element2id.getOrElseUpdate(
    self, {
      resolvedDocumentSet.element2mappedDocument( self )
        .fold[NonEmptyList[java.lang.Throwable] \/ String] {
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(self),
              "getXMI_ID error: Unknown document for element reference"))
          .left
        }{
          case d: BuiltInDocument[Uml] =>
            self
            .toolSpecific_id
            .fold[NonEmptyList[java.lang.Throwable] \/ String] {
              NonEmptyList(
                documentIDGeneratorException(
                  this,
                  Iterable(self),
                  "getXMI_ID error: Element from a BuiltInDocument without xmi:id"))
              .left
            }{ id =>
              id.right
            }

          case d: SerializableDocument[Uml] =>
            self
            .oti_xmiID
            .flatMap(oid =>
              oid.fold[(NonEmptyList[java.lang.Throwable] \/ String)]{
                computeID(self)
              }{ id =>
                \/-(id)
              })

          case d =>
            NonEmptyList(
              documentIDGeneratorException(
                this,
                Iterable(self),
                s"getXMI_ID error: Unrecognized document $d for element"))
            .left
        
      }
    })

  def computeID(self: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ String = {
    val r =
      elementRules
      .toStream
      .dropWhile { r: Element2IDRule =>
        !r.isDefinedAt(self)
      }

    if (r.nonEmpty)
      r.head(self)
    else
      self
      .owner
      .fold[NonEmptyList[java.lang.Throwable] \/ String] {
        NonEmptyList(
          documentIDGeneratorException(
            this,
            Iterable(self),
            "computeID error: Element without an owner is not supported(1)"))
        .left
      }{ owner =>
        self
        .getContainingMetaPropertyEvaluator()(this)
        .flatMap(ocf =>
          ocf.fold[NonEmptyList[java.lang.Throwable] \/ String]{
            -\/(NonEmptyList[java.lang.Throwable](documentIDGeneratorException[Uml](
              this,
              Iterable(self),
              "computeID error: Element without an owner is not supported(2)")))
          }{ cf =>
            getXMI_ID(owner)
            .flatMap{ ownerID =>
                val c = containmentRules.toStream.dropWhile((c: ContainedElement2IDRule) =>
                  !c.isDefinedAt((owner, ownerID, cf, self)))
                if (c.nonEmpty)
                  c.head((owner, ownerID, cf, self))
                else
                  -\/(NonEmptyList[java.lang.Throwable](documentIDGeneratorException(
                    this,
                    Iterable(self),
                    "computeID error: Unsupported")))
            }
          })
      }
  }

  val rule0: Element2IDRule = {
    case root: UMLPackage[Uml]
      if (resolvedDocumentSet.lookupDocumentByScope(root).isDefined
          // Adding condition for stopping the ID computation algorithm when a specification root is reached
          || root.hasSpecificationRootCharacteristics) =>
      root
      .name
      .fold[NonEmptyList[java.lang.Throwable] \/ String] {
        NonEmptyList(
          documentIDGeneratorException(
            this,
            Iterable(root),
            "rule0 error: Document package scope must be explicitly named"))
        .left
      }{
        n =>
          IDGenerator
          .xmlSafeID(n)
          .right
      }
  }

  /**
   * Rule #1 (InstanceValue)
   */
  val crule1: ContainedElement2IDRule = {
    case (owner, ownerID, cf, iv: UMLInstanceValue[Uml]) =>
      iv
      .instance
      .fold[NonEmptyList[java.lang.Throwable] \/ String] {
        NonEmptyList(
          documentIDGeneratorException(
            this,
            Iterable(owner, iv),
            "crule1 error: InstanceValue without InstanceSpecification is not supported"))
        .left
      }{ is =>
        is
        .name
        .fold[NonEmptyList[java.lang.Throwable] \/ String] {
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(owner, iv, is),
              "crule1 error: InstanceValue must refer to a named InstanceSpecification"))
          .left
        }{ nInstance =>
          iv.getContainingMetaPropertyEvaluator()(this)
          .flatMap(ocf =>
            ocf.fold[NonEmptyList[java.lang.Throwable] \/ String]{
            -\/(NonEmptyList[java.lang.Throwable](documentIDGeneratorException[Uml](
              this,
              Iterable(owner, iv, is),
              "crule1 error: Element without an owner is not supported(3)")))
          }{ cf =>
            \/-(ownerID + "_" + IDGenerator.xmlSafeID(cf.propertyName + "." + nInstance))
          })
        }
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (a): Feature or ValueSpecification
   */
  val crule1a: ContainedElement2IDRule = {
    case (owner, ownerID, cf, fv@(_: UMLFeature[Uml] | _: UMLValueSpecification[Uml])) =>
      val fvn = fv.asInstanceOf[UMLNamedElement[Uml]]
      val shortID: NonEmptyList[java.lang.Throwable] \/ String = owner match {
        case s: UMLSlot[Uml] =>
          s
          .definingFeature
          .fold[NonEmptyList[java.lang.Throwable] \/ String] {
            NonEmptyList(
              documentIDGeneratorException(
                this,
                Iterable(owner, fv),
                "crule1a error: Slot must have a defining StructuralFeature"))
            .left
          }{ sf =>
            val slotValues = s.value.toList
            if (sf.upper > 1)
              ("_" + slotValues.indexOf(fvn) + "_" + fvn.name.getOrElse(""))
              .right
            else
              fvn.name
              .getOrElse("")
              .right
          }
        case _ =>
          fvn.name.getOrElse("")
          .right

      }
      val suffix1: NonEmptyList[java.lang.Throwable] \/ String = shortID.map {
        case "" =>
          ""
        case id =>
          "." + IDGenerator.xmlSafeID(id)
      }
      val suffix2: NonEmptyList[java.lang.Throwable] \/ String = fv match {
        case bf: UMLBehavioralFeature[Uml] =>
          ( suffix1 /: bf.ownedParameter )( ( s, p ) =>
          s +++
          p._type.fold[NonEmptyList[java.lang.Throwable] \/ String]{
            NonEmptyList(
              documentIDGeneratorException(
                this,
                Iterable(owner, fv, p),
                "crule1a error: Parameter must have a type"))
            .left
          }{ t =>
            t
            .name
            .fold[NonEmptyList[java.lang.Throwable] \/ String] {
              NonEmptyList(
                documentIDGeneratorException(
                  this,
                  Iterable(owner, fv, p, t),
                  "crule1a error: Type must have a name"))
              .left
            }{ tn =>
              ( "_" + IDGenerator.xmlSafeID( tn ) )
              .right
            }
          } )
        case _ =>
          suffix1
      }
      val suffix3 =
        suffix2
        .flatMap {
          case "" =>
            (owner, owner.owner) match {
              case (s: UMLSlot[Uml], Some(is: UMLInstanceSpecification[Uml])) if cf == Slot_value =>
                s
                .definingFeature
                .fold[NonEmptyList[java.lang.Throwable] \/ String] {
                  -\/(NonEmptyList[java.lang.Throwable](documentIDGeneratorException(
                    this,
                    Iterable(is, s),
                    "crule1a error: Slot must have a defining StructuralFeature")))
                }{ sf =>
                  if (sf.upper == 1)
                    \/-("")
                  else {
                    val slotValues = s.value.toList
                    val orderedValues =
                      if (cf.isOrdered)
                        slotValues
                      else
                        slotValues.sortBy(_.xmiOrderingKey()(this).toOption.getOrElse("")) // @todo propagate errors
                    require(orderedValues.contains(fv))
                    \/-(orderedValues.indexOf(fv).toString)
                  }
                }
              case (o1, Some(o2)) =>
                require(
                  !cf.isCollection,
                  s" o1=${o1.toolSpecific_id}, o2=${o2.toolSpecific_id} /"+
                  s" o1=${getXMI_ID(o1).toOption.getOrElse("")}, o2=${getXMI_ID(o2).toOption.getOrElse("")}")
                \/-("")
              case (o1, None) =>
                require(
                  !cf.isCollection,
                  s" o1=${o1.toolSpecific_id} / o1=${getXMI_ID(o1).toOption.getOrElse("")}")
                \/-("")
            }

          case s =>
            \/-(s)

        }

      suffix3.map { s =>
        ownerID + "_" + IDGenerator.xmlSafeID(cf.propertyName + s)
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (b): not Feature, not ValueSpecification
   */
  val crule1b: ContainedElement2IDRule = {
    case (owner, ownerID, cf, ne: UMLNamedElement[Uml]) if ne.name.isDefined =>
      ( ownerID + "." +
        IDGenerator.xmlSafeID(ne.metaclass_name) + "_" +
        IDGenerator.xmlSafeID(cf.propertyName) + "_" +
        IDGenerator.xmlSafeID(ne.name.getOrElse("")))
      .right
  }

  /**
   * Rule #2:  any Element on which Rule#1 does not apply and which is owned as an ordered set
   */
  val crule2: ContainedElement2IDRule = {
    case (owner, ownerID, cf, e) if cf.isOrdered && cf.isCollection =>
      e.getElementMetamodelPropertyValue(cf)(this)
      .flatMap{ vs =>
          val values = vs.toList
          require(values.contains(e))
          \/-(ownerID + "_" + IDGenerator.xmlSafeID(cf.propertyName) + "." + values.indexOf(e))
      }
  }

  /**
   * Rule #3: any Element on which Rule#2 does not apply and which is a DirectedRelationship
   *
   * Check if the target of a directed relationship is an element in a Built-In Document
   * (e.g., UML Metamodel, StandardProfile, etc...) As of OMG UML 2.5, such target elements
   * may have a legacy xmi:ID such as "_0" which is is insufficient to avoid duplication.
   * (For example, a package that imports both UML Metamodel and StandardProfile)
   *
   * To strengthen the unicity and reproducibility of the XMI:ID of the directed relationship,
   * references to elements in built-in documents include the built-in document URI in addition to the
   * xmi:ID of the referenced element.
   */
  val crule3: ContainedElement2IDRule = {
    case (owner, ownerID, cf, dr: UMLDirectedRelationship[Uml]) =>
      dr.target.toList match {
        case List(relTarget) =>
          getXMI_IDREF_or_HREF_fragment(owner, relTarget)
          .flatMap { tid =>
              val targetID =
                resolvedDocumentSet.element2mappedDocument(relTarget) match {
                  case Some(d: BuiltInDocument[Uml]) =>
                    IDGenerator.xmlSafeID(d.uri.toString() + "." + tid)
                  case _ =>
                    tid
                }

              \/-(ownerID + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + targetID)
          }
        case _ =>
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(owner, dr),
              "crule3 error: Binary DirectedRelationship must have a target"))
          .left
      }
  }

  /**
   * Rule #4: any Element on which Rule#3 does not apply and which is a uml::Slot
   */
  val crule4: ContainedElement2IDRule = {
    case (owner, ownerID, cf, s: UMLSlot[Uml]) =>
      s
      .definingFeature
      .fold[NonEmptyList[java.lang.Throwable] \/ String] {
        NonEmptyList(
          documentIDGeneratorException(
            this,
            Iterable(owner, s),
            "crule4 error: Slot must have a defining StructuralFeature"))
        .left
      }{ sf =>
        sf
        .name
        .fold[NonEmptyList[java.lang.Throwable] \/ String] {
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(owner, s),
              "crule4 error: Slot's defining StructuralFeature must be named"))
          .left
        }{ sfn =>
          (ownerID + "." + IDGenerator.xmlSafeID(sfn))
          .right
        }
      }
  }

  /**
   * Rule #5: any Element on which Rule#4 does not apply and which is uml::Comment
   */
  val crule5: ContainedElement2IDRule = {
    case (owner, ownerID, cf, c: UMLComment[Uml]) =>
      (ownerID + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + c.getCommentOwnerIndex)
      .right
  }

  /**
   * Rule #6: any Element on which Rule#5 does not apply and which is uml::Image
   */
  val crule6: ContainedElement2IDRule = {
    case (owner, ownerID, cf, i: UMLImage[Uml]) =>
      getImageLocationURL(i) map { locationURL =>
        ownerID + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + IDGenerator.xmlSafeID(locationURL)
      }
  }

  def getImageLocationURL(i: UMLImage[Uml]): NonEmptyList[java.lang.Throwable] \/ String =
    i
    .location
    .fold[NonEmptyList[java.lang.Throwable] \/ String] {
      NonEmptyList(
        documentIDGeneratorException(
          this,
          Iterable(i),
          "getImageLocationURL error: An Image must have a non-null location URL"))
      .left
    }{ loc =>
        catching(classOf[java.net.MalformedURLException], classOf[java.lang.SecurityException])
        .withApply { cause: java.lang.Throwable =>
          NonEmptyList(
            documentIDGeneratorException(
              this,
              Iterable(i),
              "getImageLocationURL error",
              cause))
            .left
        }
        .apply({
          val url = new URL(loc) toString;
          IDGenerator
          .getValidNCName(url)
          .right
        })
    }

  def checkIDs(): Boolean = {
    val id2Element = scala.collection.mutable.HashMap[String, UMLElement[Uml]]()
    var res: Boolean = true
    var duplicates: Integer = 0
    var failed: Integer = 0
    println("\n>>> IDs Checking...")

    val res0: Boolean = true
    val resN: Boolean = ( res0 /: getElement2IDMap ) {
      case ( resi, (ei, maybeId)) =>
      resi &&
      maybeId
      .fold[Boolean](
        (errors: NonEmptyList[java.lang.Throwable]) => {
          failed = failed + 1
          println(s"***ID computation failed for ${ei.toWrappedObjectString}")
          for {
            error <- errors
          } println("\tCause: " + error.getMessage)
          println("---------------------------")
          false
        },

        (id: String) => {
            id2Element
            .get(id)
            .fold[Boolean]({
              id2Element.update(id, ei)
              true
            }){ e =>
              if (e == ei)
                true
              else {
                duplicates = duplicates + 1
                println(s"*** Duplicate ID: $id")
                println(s"\t-> ${ei.toWrappedObjectString}")
                println(s"\t-> ${e.toWrappedObjectString}")
                println("---------------------------")
                false
              }
            }
        }
      )
    }
    println(s"<<<... IDs Checked ($duplicates duplicates, $failed failed)\n")
    resN
  } // end CheckIds

  def listIDs(): Unit = {
    println(s"element2id size=${element2id.size}")
    for ((x, id) <- element2id) {
      val idStr =
        id
        .fold[String](
          (errors: NonEmptyList[java.lang.Throwable]) =>
          s"*** Fail: ${errors.size} erors",
          (s: String) =>
          s)
      x match {
        case ne: UMLNamedElement[Uml] =>
          val nameStr =
            ne.name
            .fold("(unnamed)") { identity _ }
          println("<" + ne.mofMetaclassName + ">" + nameStr + "\t=> ID: " + idStr)

        case e: UMLElement[Uml] =>
          println("<" + e.mofMetaclassName + ">\t=> ID: " + idStr)
      }
    }
  }

}
