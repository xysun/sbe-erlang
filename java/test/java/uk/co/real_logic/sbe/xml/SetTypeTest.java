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
import uk.co.real_logic.sbe.TestUtil;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.Integer.valueOf;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.parse;

public class SetTypeTest
{
    @Test
    public void shouldHandleBinarySetType()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"biOp\" encodingType=\"uint8\">" +
            "    <choice name=\"Bit0\" description=\"Bit 0\">0</choice>" +
            "    <choice name=\"Bit1\" description=\"Bit 1\">1</choice>" +
            "</set>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/set", testXmlString);
        SetType e = (SetType)map.get("biOp");
        assertThat(e.name(), is("biOp"));
        assertThat(e.encodingType(), is(PrimitiveType.UINT8));
        assertThat(valueOf(e.choices().size()), is(valueOf(2)));
        assertThat(e.getChoice("Bit1").primitiveValue(), is(PrimitiveValue.parse("1", PrimitiveType.UINT8)));
        assertThat(e.getChoice("Bit0").primitiveValue(), is(PrimitiveValue.parse("0", PrimitiveType.UINT8)));
    }

    @Test
    public void shouldHandleSetTypeList()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"listed\" encodingType=\"uint8\">" +
            "    <choice name=\"Bit0\">0</choice>" +
            "    <choice name=\"Bit1\">1</choice>" +
            "    <choice name=\"Bit2\">2</choice>" +
            "    <choice name=\"Bit3\">3</choice>" +
            "</set>" +
            "</types>";

        Map<String, Type> map = parseTestXmlWithMap("/types/set", testXmlString);
        SetType e = (SetType)map.get("listed");
        assertThat(e.encodingType(), is(PrimitiveType.UINT8));

        int foundBit0 = 0, foundBit1 = 0, foundBit2 = 0, foundBit3 = 0, count = 0;
        for (final SetType.Choice choice : e.choices())
        {
            if (choice.name().equals("Bit0"))
            {
                foundBit0++;
            }
            else if (choice.name().equals("Bit1"))
            {
                foundBit1++;
            }
            else if (choice.name().equals("Bit2"))
            {
                foundBit2++;
            }
            else if (choice.name().equals("Bit3"))
            {
                foundBit3++;
            }
            count++;
        }

        assertThat(valueOf(count), is(valueOf(4)));
        assertThat(valueOf(foundBit0), is(valueOf(1)));
        assertThat(valueOf(foundBit1), is(valueOf(1)));
        assertThat(valueOf(foundBit2), is(valueOf(1)));
        assertThat(valueOf(foundBit3), is(valueOf(1)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowExceptionWhenIllegalEncodingTypeSpecified()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"biOp\" encodingType=\"char\">" +
            "    <choice name=\"Bit0\">0</choice>" +
            "    <choice name=\"Bit1\">1</choice>" +
            "</set>" +
            "</types>";

        parseTestXmlWithMap("/types/set", testXmlString);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowExceptionWhenDuplicateValueSpecified()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"biOp\" encodingType=\"uint8\">" +
            "    <choice name=\"Bit0\">0</choice>" +
            "    <choice name=\"AnotherBit0\">0</choice>" +
            "</set>" +
            "</types>";

        parseTestXmlWithMap("/types/set", testXmlString);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowExceptionWhenDuplicateNameSpecified()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"biOp\" encodingType=\"uint8\">" +
            "    <choice name=\"Bit0\">0</choice>" +
            "    <choice name=\"Bit0\">1</choice>" +
            "</set>" +
            "</types>";

        parseTestXmlWithMap("/types/set", testXmlString);
    }

    @Test(expected = IllegalArgumentException.class)
    public void shouldThrowExceptionWhenValueOutOfBoundsSpecified()
        throws Exception
    {
        final String testXmlString =
            "<types>" +
            "<set name=\"biOp\" encodingType=\"uint8\">" +
            "    <choice name=\"Bit0\">0</choice>" +
            "    <choice name=\"Bit100\">100</choice>" +
            "</set>" +
            "</types>";

        parseTestXmlWithMap("/types/set", testXmlString);
    }

     @Test
     public void shouldHandleEncodingTypesWithNamedTypes()
         throws Exception
     {
         MessageSchema schema = parse(TestUtil.getLocalResource("encoding-types-schema.xml"));
         List<Field> fields = schema.getMessage(1).fields();
         assertNotNull(fields);
         SetType type = (SetType)fields.get(3).type();
         assertThat(type.encodingType(), is(PrimitiveType.UINT8));
         type = (SetType)fields.get(4).type();
         assertThat(type.encodingType(), is(PrimitiveType.UINT16));
         type = (SetType)fields.get(5).type();
         assertThat(type.encodingType(), is(PrimitiveType.UINT32));
         type = (SetType)fields.get(6).type();
         assertThat(type.encodingType(), is(PrimitiveType.UINT64));
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
        System.setProperty(SbeTool.VALIDATION_WARNINGS_FATAL, "true");
        document.setUserData(XmlSchemaParser.ERROR_HANDLER_KEY, new ErrorHandler(), null);

        for (int i = 0, size = list.getLength(); i < size; i++)
        {
            Type t = new SetType(list.item(i));
            map.put(t.name(), t);
        }

        return map;
    }
}
