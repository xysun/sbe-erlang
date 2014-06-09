/* -*- mode: java; c-basic-offset: 4; tab-width: 4; indent-tabs-mode: nil -*- */
/*
 * Copyright 2013 Real Logic Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package uk.co.real_logic.sbe.xml;

import uk.co.real_logic.sbe.SbeTool;

import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import uk.co.real_logic.sbe.PrimitiveType;
import uk.co.real_logic.sbe.PrimitiveValue;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static java.lang.Integer.valueOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class CompositeTypeTest
{
    @Test
    public void shouldHandleDecimalCompositeType()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<composite name=\"decimal\">" +
            "    <type name=\"mantissa\" primitiveType=\"int64\"/>" +
            "    <type name=\"exponent\" primitiveType=\"int8\"/>" +
            "</composite>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/composite", testXmlString);
        CompositeType decimal = (CompositeType)map.get("decimal");
        assertThat(decimal.name(), is("decimal"));
        assertThat(decimal.getType("mantissa").primitiveType(), is(PrimitiveType.INT64));
        assertThat(decimal.getType("exponent").primitiveType(), is(PrimitiveType.INT8));
        assertThat(valueOf(decimal.size()), is(valueOf(9)));
    }

    @Test
    public void shouldHandleDecimal32CompositeType()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<composite name=\"decimal32\">" +
            "    <type name=\"mantissa\" primitiveType=\"int32\"/>" +
            "    <type name=\"exponent\" primitiveType=\"int8\" presence=\"constant\">-2</type>" +
            "</composite>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/composite", testXmlString);
        CompositeType decimal32 = (CompositeType)map.get("decimal32");
        assertThat(decimal32.name(), is("decimal32"));
        assertThat(decimal32.getType("mantissa").primitiveType(), is(PrimitiveType.INT32));
        assertThat(decimal32.getType("exponent").primitiveType(), is(PrimitiveType.INT8));
        assertThat(decimal32.getType("exponent").presence(), is(Presence.CONSTANT));
        assertThat((decimal32.getType("exponent")).constVal(), is(PrimitiveValue.parse("-2", PrimitiveType.INT8)));
        assertThat(valueOf(decimal32.size()), is(valueOf(4)));
    }

    @Test
    public void shouldHandleDecimal64CompositeType()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<composite name=\"decimal64\">" +
            "    <type name=\"mantissa\" primitiveType=\"int64\"/>" +
            "    <type name=\"exponent\" primitiveType=\"int8\" presence=\"constant\">-2</type>" +
            "</composite>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/composite", testXmlString);
        CompositeType decimal64 = (CompositeType)map.get("decimal64");
        assertThat(decimal64.name(), is("decimal64"));
        assertThat(decimal64.getType("mantissa").primitiveType(), is(PrimitiveType.INT64));
        assertThat(decimal64.getType("exponent").primitiveType(), is(PrimitiveType.INT8));
        assertThat(decimal64.getType("exponent").presence(), is(Presence.CONSTANT));
        assertThat((decimal64.getType("exponent")).constVal(), is(PrimitiveValue.parse("-2", PrimitiveType.INT8)));
        assertThat(valueOf(decimal64.size()), is(valueOf(8)));
    }

    @Test
    public void shouldHandleCompositeTypeList()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<composite name=\"decimal\">" +
            "    <type name=\"mantissa\" primitiveType=\"int64\"/>" +
            "    <type name=\"exponent\" primitiveType=\"int8\"/>" +
            "</composite>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/composite", testXmlString);
        CompositeType c = (CompositeType)map.get("decimal");
        assertThat(valueOf(c.getTypeList().size()), is(valueOf(2)));
        assertThat(c.getTypeList().get(0).name(), is("mantissa"));
        assertThat(c.getTypeList().get(1).name(), is("exponent"));
    }

    @Test
    public void shouldHandleCompositeHasNullableType()
        throws Exception
    {
        final String nullValStr = "9223372036854775807";
        final String testXmlString =
            "<types>" +
            "<composite name=\"PRICENULL\" description=\"Price NULL\" semanticType=\"Price\">" +
            "    <type name=\"mantissa\" description=\"mantissa\" presence=\"optional\" nullValue=\"" + nullValStr + "\" primitiveType=\"int64\"/>" +
            "    <type name=\"exponent\" description=\"exponent\" presence=\"constant\" primitiveType=\"int8\">-7</type>" +
            "</composite>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/composite", testXmlString);
        CompositeType c = (CompositeType)map.get("PRICENULL");
        assertThat((c.getType("mantissa")).nullValue(), is(PrimitiveValue.parse(nullValStr, PrimitiveType.INT64)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowExceptionWhenCompositeTypeHasTypeNameDuplicates()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<composite name=\"decimal\">" +
            "    <type name=\"mantissa\" primitiveType=\"int64\"/>" +
            "    <type name=\"mantissa\" primitiveType=\"int64\"/>" +
            "    <type name=\"exponent\" primitiveType=\"int8\"/>" +
            "</composite>" +
            "</types>";

        parseTestXmlWithMap("/types/composite", testXmlString);
    }

    private static Map<String, Type> parseTestXmlWithMap(final String xPathExpr, final String xml)
        throws ParserConfigurationException, XPathExpressionException, IOException, SAXException
    {
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new ByteArrayInputStream(xml.getBytes()));
        XPath xPath = XPathFactory.newInstance().newXPath();
        NodeList list = (NodeList)xPath.compile(xPathExpr).evaluate(document, XPathConstants.NODESET);
        Map<String, Type> map = new HashMap<>();

        System.setProperty(SbeTool.VALIDATION_STOP_ON_ERROR, "true");
        System.setProperty(SbeTool.VALIDATION_SUPPRESS_OUTPUT, "true");
        document.setUserData(XmlSchemaParser.ERROR_HANDLER_KEY, new ErrorHandler(), null);

        for (int i = 0, size = list.getLength(); i < size; i++)
        {
            Type t = new CompositeType(list.item(i));
            map.put(t.name(), t);
        }

        return map;
    }
}
