//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2018.06.11 at 01:21:37 AM CEST 
//


package org.knowrob.vis.collada_1_4_1;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for fx_opaque_enum.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="fx_opaque_enum">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="A_ONE"/>
 *     &lt;enumeration value="RGB_ZERO"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "fx_opaque_enum")
@XmlEnum
public enum FxOpaqueEnum {


    /**
     * 
     *                         When a transparent opaque attribute is set to A_ONE, it means the transparency information will be taken from the alpha channel of the color, texture, or parameter supplying the value. The value of 1.0 is opaque in this mode.
     *                     
     * 
     */
    A_ONE,

    /**
     * 
     *                         When a transparent opaque attribute is set to RGB_ZERO, it means the transparency information will be taken from the red, green, and blue channels of the color, texture, or parameter supplying the value. Each channel is modulated independently. The value of 0.0 is opaque in this mode.
     *                     
     * 
     */
    RGB_ZERO;

    public String value() {
        return name();
    }

    public static FxOpaqueEnum fromValue(String v) {
        return valueOf(v);
    }

}
