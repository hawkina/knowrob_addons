//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.8-b130911.1802 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2018.06.11 at 01:21:37 AM CEST 
//


package org.knowrob.vis.collada_1_4_1;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for gl_fog_type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="gl_fog_type">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="LINEAR"/>
 *     &lt;enumeration value="EXP"/>
 *     &lt;enumeration value="EXP2"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "gl_fog_type")
@XmlEnum
public enum GlFogType {

    LINEAR("LINEAR"),
    EXP("EXP"),
    @XmlEnumValue("EXP2")
    EXP_2("EXP2");
    private final String value;

    GlFogType(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static GlFogType fromValue(String v) {
        for (GlFogType c: GlFogType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
