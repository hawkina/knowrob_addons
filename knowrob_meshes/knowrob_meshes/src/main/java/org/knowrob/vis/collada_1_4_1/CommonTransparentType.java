//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2018.06.11 at 01:21:37 AM CEST 
//


package org.knowrob.vis.collada_1_4_1;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for common_transparent_type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="common_transparent_type">
 *   &lt;complexContent>
 *     &lt;extension base="{http://www.collada.org/2005/11/COLLADASchema}common_color_or_texture_type">
 *       &lt;attribute name="opaque" type="{http://www.collada.org/2005/11/COLLADASchema}fx_opaque_enum" default="A_ONE" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "common_transparent_type")
public class CommonTransparentType
    extends CommonColorOrTextureType
{

    @XmlAttribute(name = "opaque")
    protected FxOpaqueEnum opaque;

    /**
     * Gets the value of the opaque property.
     * 
     * @return
     *     possible object is
     *     {@link FxOpaqueEnum }
     *     
     */
    public FxOpaqueEnum getOpaque() {
        if (opaque == null) {
            return FxOpaqueEnum.A_ONE;
        } else {
            return opaque;
        }
    }

    /**
     * Sets the value of the opaque property.
     * 
     * @param value
     *     allowed object is
     *     {@link FxOpaqueEnum }
     *     
     */
    public void setOpaque(FxOpaqueEnum value) {
        this.opaque = value;
    }

}
