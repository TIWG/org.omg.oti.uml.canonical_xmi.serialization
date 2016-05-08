/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.canonicalXMI

import java.lang.Integer
import java.net.URL

import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils

import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi._

import scala.{Boolean, Int, None, Option, Some, StringContext, Unit}
import scala.Predef.{require, Map => _, Set => _, _}
import scala.collection.immutable._
import scala.language.postfixOps
import scala.util.control.Exception._
import scalaz.Scalaz._
import scalaz._

/**
 * @tparam Uml
 */
trait DocumentHashIDGenerator[Uml <: UML] extends IDGenerator[Uml] {

  implicit val documentSet: DocumentSet[Uml]

  override implicit val umlOps = documentSet.ops

  import umlOps._

  implicit val documentOps: DocumentOps[Uml] = documentSet.documentOps

  override implicit val otiCharacteristicsProvider
  : OTICharacteristicsProvider[Uml]
  = documentOps.otiCharacteristicsProvider

  protected val element2id: Element2IDHashMap

  protected val elementRules: List[Element2UUIDRule]

  protected val containmentRules: List[ContainedElement2UUIDRule]

  protected val element2uuid: Element2UUIDHashMap

  def element2document
  (e: UMLElement[Uml])
  : Option[Document[Uml]]
  = documentSet.lookupDocumentByExtent(e)

  override def element2mappedDocument
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[Document[Uml]]
  = element2document(e).right

  override def getElement2IDMap
  : Map[UMLElement[Uml], (Set[java.lang.Throwable] \/ (String @@ OTI_ID))]
  = element2id.toMap

  override def getElement2UUIDMap
  : Map[UMLElement[Uml], (Set[java.lang.Throwable] \/ (String @@ OTI_UUID))]
  = element2uuid.toMap

  override def lookupElementXMI_ID
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[String @@ OTI_ID]
  = element2id
    .get(e)
    .fold[Set[java.lang.Throwable] \/ Option[String @@ OTI_ID]](
      Option.empty[String @@ OTI_ID].right
    ){ id =>
      id.map(_.some)
    }

  override def lookupElementXMI_UUID
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Option[String @@ OTI_UUID]
  = element2uuid
      .get(e)
      .fold[Set[java.lang.Throwable] \/ Option[String @@ OTI_UUID]](
      Option.empty[String @@ OTI_UUID].right
    ){ uuid =>
      uuid.map(_.some)
    }

  /**
   * Computes the xmi:ID for each element in the domain of the element2document map of the ResolvedDocumentSet
   */
  def computePackageExtentXMI_ID(pkg: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Unit
  = {

    val extent = pkg
      .allOwnedElements
      .+(pkg)
      .filter(element2document(_).isDefined)

    val e0: Set[java.lang.Throwable] \/ Unit = ().right
    val eN: Set[java.lang.Throwable] \/ Unit = (e0 /: extent) {
      (ei, e) =>
        ei +++ getXMI_ID(e).map( _ => ())
    }

    eN
  }

  /**
    * Computes the xmi:UUID for each element in the domain of the element2document map of the ResolvedDocumentSet
    */
  def computePackageExtentXMI_UUID(pkg: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Unit
  = {
    val extent = pkg
      .allOwnedElements
      .+(pkg)
      .filter(element2document(_).isDefined)

    val e0: Set[java.lang.Throwable] \/ Unit = ().right
    val eN: Set[java.lang.Throwable] \/ Unit = (e0 /: extent) {
      (ei, e) =>
        ei +++ getXMI_UUID(e).map( _ => ())
    }

    eN
  }

  protected def getXMI_IDREF_or_HREF_fragment
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ String
  = getXMI_IDREF_or_HREF_fragment_internal(from, to)
    .orElse(
      getMappedOrReferencedElement(to)
        .flatMap { _to =>
          getXMI_IDREF_or_HREF_fragment(from, _to)
        })

  protected def getXMI_IDREF_or_HREF_fragment_internal
  (from: UMLElement[Uml],
   to: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ String
  = element2document(from)
    .fold[Set[java.lang.Throwable] \/ String] {
      Set(
        UMLError
        .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference from", Iterable(from)))
      .left
    }{ d1 =>
      element2document(to)
      .fold[Set[java.lang.Throwable] \/ String] {
        Set(
          UMLError
          .illegalElementError[Uml, UMLElement[Uml]]("Unknown document for element reference to", Iterable(to)))
        .left
      }{
        case db2: Document[Uml] with BuiltInDocument =>
          require(d1 != db2)
          val builtIn_d2_id = TOOL_SPECIFIC_ID.unwrap(to.toolSpecific_id)
          // Based on the built-in 'to' element ID, construct the built-in URI for the 'to' element.
          val bid = for {
            builtInURI <- documentOps.getExternalDocumentURL(db2.documentURL)

            builtInURITo =
              builtInURI
              .resolve("#" + builtIn_d2_id)
              .toString

            // use the builtInURIMapper to convert the built-in URI of the 'to' element into an OMG URI
            mappedURITo <- documentSet.builtInURIMapper.resolve(builtInURITo).map(_.getOrElse(builtInURITo))
            fragmentIndex = mappedURITo.lastIndexOf('#')
            _ = require(fragmentIndex > 0)

            // It's not needed to add the prefix since it's already included in the computed ID
            fragment = IDGenerator.xmlSafeID(mappedURITo.substring(fragmentIndex + 1))
          } yield IDGenerator.xmlSafeID(fragment)

          bid

        case _: Document[Uml] with SerializableDocument =>
          getMappedOrReferencedElement(to)
          .flatMap( _to => getXMI_UUID(_to).map(OTI_UUID.unwrap))

        case d =>
          Set(
            documentUUIDGeneratorException(
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
  : Set[java.lang.Throwable] \/ (String @@ OTI_ID)
  = element2id.getOrElseUpdate(
    self, {
      element2document( self )
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)] {
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(self),
              "getXMI_ID error: Unknown document for element reference"))
          .left
        }{
          case d: Document[Uml] with BuiltInDocument =>
            OTI_ID.apply(TOOL_SPECIFIC_ID.unwrap(self.toolSpecific_id)).right

          case d: Document[Uml] with SerializableDocument =>
            self
            .oti_xmiID
            .flatMap(oid =>
              oid.fold[Set[java.lang.Throwable] \/ (String @@ OTI_ID)]{
                computeSingleElementXMI_ID(self)
              }{ id =>
                id.right
              })

          case d =>
            Set(
              documentUUIDGeneratorException(
                this,
                Iterable(self),
                s"getXMI_ID error: Unrecognized document $d for element"))
            .left
        
      }
    })

  def computeSingleElementXMI_ID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_ID)
  = self
    .xmiUUID()(this)
    .map { uuid =>
      val id = Hex.encodeHexString(DigestUtils.sha(OTI_UUID.unwrap(uuid)))
      OTI_ID(id)
    }

  /**
    * The xmi:UUID of an element depends on what kind of document it is contained in.
    * - BuiltInDocument: this is deferred to builtInUUID, which is implementation-specific.
    * - SerializableDocument: this is the OTI implementation of Canonical XMI UUID
    * unless it is overriden by an application of the OTI::Identity stereotype
    */
  def getXMI_UUID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_UUID)
  = element2uuid.getOrElseUpdate(
      self, {
        element2document( self )
          .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(self),
              "getXMI_UUID error: Unknown document for element reference"))
            .left
        }{
          case d: Document[Uml] with BuiltInDocument =>
            self
              .toolSpecific_uuid
              .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
              Set(
                documentUUIDGeneratorException(
                  this,
                  Iterable(self),
                  "getXMI_UUID error: Element from a BuiltInDocument without xmi:uuid"))
                .left
            }{ uuid =>
              OTI_UUID.apply(TOOL_SPECIFIC_UUID.unwrap(uuid)).right
            }

          case d: Document[Uml] with SerializableDocument =>
            self
              .oti_xmiUUID
              .flatMap(ouuid =>
                ouuid.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)]{
                  computeSingleElementXMI_UUID(self)
                }{ uuid =>
                  uuid.right
                })

          case d =>
            Set(
              documentUUIDGeneratorException(
                this,
                Iterable(self),
                s"getXMI_UUID error: Unrecognized document $d for element"))
              .left

        }
      })

  def computeSingleElementXMI_UUID(self: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_UUID)
  = {
    element2mappedDocument(self)
      .flatMap {
        _.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)](
          Set(
            UMLError
              .illegalElementError[Uml, UMLElement[Uml]](
              s"Cannot generate the OTI uuid for $self because it does not belong to a document",
              Iterable(self)))
            .left
        ) { d =>
          val r =
            elementRules
              .toStream
              .dropWhile { r: Element2UUIDRule =>
                !r.isDefinedAt(self)
              }

          val ouuid
          : Set[java.lang.Throwable] \/ (String @@ OTI_UUID)
          = if (r.nonEmpty)
            r.head(self)
          else
            self
              .owner
              .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
              Set(
                documentUUIDGeneratorException(
                  this,
                  Iterable(self),
                  "computeID error: Element without an owner is not supported(1)"))
                .left
            } { owner =>
              self
                .getContainingMetaPropertyEvaluator()(this)
                .flatMap(ocf =>
                  ocf.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
                    -\/(Set[java.lang.Throwable](documentUUIDGeneratorException[Uml](
                      this,
                      Iterable(self),
                      "computeID error: Element without an owner is not supported(2)")))
                  } { cf =>
                    getXMI_UUID(owner)
                      .flatMap { ownerUUID =>
                        val c = containmentRules.toStream.dropWhile((c: ContainedElement2UUIDRule) =>
                          !c.isDefinedAt((owner, ownerUUID, cf, self)))
                        if (c.nonEmpty)
                          c.head((owner, ownerUUID, cf, self))
                        else
                          -\/(Set[java.lang.Throwable](documentUUIDGeneratorException(
                            this,
                            Iterable(self),
                            "computeID error: Unsupported")))
                      }
                  })
            }

          ouuid
            .flatMap { uuid =>
              OTI_UUID(d.info.uuidPrefix + OTI_UUID.unwrap(uuid))
                .right
            }
        }
      }
  }

  val rule0: Element2UUIDRule = {
    case root: UMLPackage[Uml]
      if (documentSet.lookupDocumentByScope(root).isDefined
          // Adding condition for stopping the ID computation algorithm when a specification root is reached
          || root.hasSpecificationRootCharacteristics) =>
      root
      .name
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
        Set(
          documentUUIDGeneratorException(
            this,
            Iterable(root),
            "rule0 error: Document package scope must be explicitly named"))
        .left
      }{
        n =>
          val uuid = IDGenerator.xmlSafeID(n)
          OTI_UUID(uuid).right
      }
  }

  /**
   * Rule #1 (InstanceValue)
   */
  val crule1: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, iv: UMLInstanceValue[Uml]) =>
      iv
      .instance
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
        Set(
          documentUUIDGeneratorException(
            this,
            Iterable(owner, iv),
            "crule1 error: InstanceValue without InstanceSpecification is not supported"))
        .left
      }{ is =>
        is
        .name
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(owner, iv, is),
              "crule1 error: InstanceValue must refer to a named InstanceSpecification"))
          .left
        }{ nInstance =>
          iv.getContainingMetaPropertyEvaluator()(this)
          .flatMap(ocf =>
            ocf.fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)]{
            -\/(Set[java.lang.Throwable](documentUUIDGeneratorException[Uml](
              this,
              Iterable(owner, iv, is),
              "crule1 error: Element without an owner is not supported(3)")))
          }{ cf =>
              val uuid =
                OTI_UUID.unwrap(ownerUUID) + "_" + IDGenerator.xmlSafeID(cf.propertyName + "." + nInstance)
              \/-(OTI_UUID(uuid))
          })
        }
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (a): Feature or ValueSpecification
   */
  val crule1a: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, fv@(_: UMLFeature[Uml] | _: UMLValueSpecification[Uml])) =>
      val fvn = fv.asInstanceOf[UMLNamedElement[Uml]]
      val shortID: Set[java.lang.Throwable] \/ String = owner match {
        case s: UMLSlot[Uml] =>
          s
          .definingFeature
          .fold[Set[java.lang.Throwable] \/ String] {
            Set(
              documentUUIDGeneratorException(
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
      val suffix1: Set[java.lang.Throwable] \/ String = shortID.map {
        case "" =>
          ""
        case id =>
          "." + IDGenerator.xmlSafeID(id)
      }
      val suffix2: Set[java.lang.Throwable] \/ String = fv match {
        case bf: UMLBehavioralFeature[Uml] =>
          ( suffix1 /: bf.ownedParameter )( ( s, p ) =>
          s +++
          p._type.fold[Set[java.lang.Throwable] \/ String]{
            Set(
              documentUUIDGeneratorException(
                this,
                Iterable(owner, fv, p),
                "crule1a error: Parameter must have a type"))
            .left
          }{ t =>
            t
            .name
            .fold[Set[java.lang.Throwable] \/ String] {
              Set(
                documentUUIDGeneratorException(
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
                .fold[Set[java.lang.Throwable] \/ String] {
                  -\/(Set[java.lang.Throwable](documentUUIDGeneratorException(
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
                        slotValues
                        .sortBy(
                          _
                            .xmiOrderingKey()(this)
                            .toOption
                            .getOrElse("")) // @todo propagate errors
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
        OTI_UUID(ownerUUID + "_" + IDGenerator.xmlSafeID(cf.propertyName + s))
      }
  }

  /**
   * Rule #1 (NamedElement)
   * case (b): not Feature, not ValueSpecification
   */
  val crule1b: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, ne: UMLNamedElement[Uml]) if ne.name.isDefined =>
      OTI_UUID(
        OTI_UUID.unwrap(ownerUUID) + "." +
        IDGenerator.xmlSafeID(ne.metaclass_name) + "_" +
        IDGenerator.xmlSafeID(cf.propertyName) + "_" +
        IDGenerator.xmlSafeID(ne.name.getOrElse("")) )
      .right
  }

  /**
   * Rule #2:  any Element on which Rule#1 does not apply and which is owned as an ordered set
   */
  val crule2: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, e) if cf.isOrdered && cf.isCollection =>
      e.getElementMetamodelPropertyValue(cf)(this)
      .flatMap{ vs =>
        val values = vs.toList
        require(values.contains(e))
        val uuid =
          OTI_UUID.unwrap(ownerUUID) + "_" + IDGenerator.xmlSafeID(cf.propertyName) + "." + values.indexOf(e)
        \/-(OTI_UUID(uuid))
      }
  }

  /**
   * Rule #3: any Element on which Rule#2 does not apply and which is a DirectedRelationship
   *
   * Check if the source & target of a directed relationship are elements of a Built-In Document
   * (e.g., UML Metamodel, StandardProfile, etc...) As of OMG UML 2.5, such source/target elements
   * may have a legacy xmi:ID such as "_0" which is is insufficient to avoid duplication.
   * (For example, a package that imports both UML Metamodel and StandardProfile)
   *
   * To strengthen the unicity and reproducibility of the XMI:ID of the directed relationship,
   * references to elements in built-in documents include the built-in document URI in addition to the
   * xmi:ID of the referenced element.
   */
  val crule3: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, dr: UMLDirectedRelationship[Uml]) =>
      (dr.source.toList, dr.target.toList) match {
        case (List(relSource), List(relTarget)) =>
          getXMI_IDREF_or_HREF_fragment(owner, relSource)
            .flatMap { sid =>
              getXMI_IDREF_or_HREF_fragment(owner, relTarget)
                .flatMap { tid =>

                  val sourceID =
                    element2document(relSource) match {
                      case Some(d: Document[Uml] with BuiltInDocument) =>
                        OTI_ID(IDGenerator.xmlSafeID(OTI_URI.unwrap(d.info.packageURI) + "." + sid))
                      case _ =>
                        sid
                    }

                  val targetID =
                    element2document(relTarget) match {
                      case Some(d: Document[Uml] with BuiltInDocument) =>
                        OTI_ID(IDGenerator.xmlSafeID(OTI_URI.unwrap(d.info.packageURI) + "." + tid))
                      case _ =>
                        tid
                    }

                  val relUUID =
                    OTI_UUID.unwrap(ownerUUID) +
                      "." + sourceID +
                      "._" + IDGenerator.xmlSafeID(cf.propertyName) +
                      "._" + dr.mofMetaclassName +
                      "." + targetID

                  \/-(OTI_UUID(relUUID))
                }
            }
        case _ =>
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(owner, dr),
              "crule3 error: Binary DirectedRelationship must have a target"))
          .left
      }
  }

  /**
   * Rule #4: any Element on which Rule#3 does not apply and which is a uml::Slot
   */
  val crule4: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, s: UMLSlot[Uml]) =>
      s
      .definingFeature
      .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
        Set(
          documentUUIDGeneratorException(
            this,
            Iterable(owner, s),
            "crule4 error: Slot must have a defining StructuralFeature"))
        .left
      }{ sf =>
        sf
        .name
        .fold[Set[java.lang.Throwable] \/ (String @@ OTI_UUID)] {
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(owner, s),
              "crule4 error: Slot's defining StructuralFeature must be named"))
          .left
        }{ sfn =>
          val uuid =
            OTI_UUID.unwrap(ownerUUID) + "." + IDGenerator.xmlSafeID(sfn)
          OTI_UUID(uuid).right
        }
      }
  }

  /**
   * Rule #5: any Element on which Rule#4 does not apply and which is uml::Comment
   */
  val crule5: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, c: UMLComment[Uml]) =>
      val uuid =
        OTI_UUID.unwrap(ownerUUID) + "._" + IDGenerator.xmlSafeID(cf.propertyName) + "." + c.getCommentOwnerIndex
      OTI_UUID(uuid).right
  }

  /**
   * Rule #6: any Element on which Rule#5 does not apply and which is uml::Image
   */
  val crule6: ContainedElement2UUIDRule = {
    case (owner, ownerUUID, cf, i: UMLImage[Uml]) =>
      getImageLocationURL(i) map { locationURL =>
        val uuid =
          OTI_UUID.unwrap(ownerUUID) +
            "._" + IDGenerator.xmlSafeID(cf.propertyName) +
            "." + IDGenerator.xmlSafeID(OTI_URL.unwrap(locationURL))
        OTI_UUID(uuid)
      }
  }

  def getImageLocationURL(i: UMLImage[Uml])
  : Set[java.lang.Throwable] \/ (String @@ OTI_URL)
  = i
    .location
    .fold[Set[java.lang.Throwable] \/ (String @@ OTI_URL)] {
      Set(
        documentUUIDGeneratorException(
          this,
          Iterable(i),
          "getImageLocationURL error: An Image must have a non-null location URL"))
      .left
    }{ loc =>
        nonFatalCatch
        .withApply { cause: java.lang.Throwable =>
          Set(
            documentUUIDGeneratorException(
              this,
              Iterable(i),
              "getImageLocationURL error",
              cause))
            .left
        }
        .apply({
          val url = new URL(loc) toString;
          OTI_URL(IDGenerator.getValidNCName(url))
          .right
        })
    }

  def checkIDs(): Boolean = {
    val id2Element = scala.collection.mutable.HashMap[String @@ OTI_ID, UMLElement[Uml]]()
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
        (errors: Set[java.lang.Throwable]) => {
          failed = failed + 1
          println(s"***ID computation failed for ${ei.toWrappedObjectString}")
          for {
            error <- errors
          } println("\tCause: " + error.getMessage)
          println("---------------------------")
          false
        },

        (id: String @@ OTI_ID) => {
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
          (errors: Set[java.lang.Throwable]) =>
          s"*** Fail: ${errors.size} erors",
          (s: String @@ OTI_ID) =>
          OTI_ID.unwrap(s))
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