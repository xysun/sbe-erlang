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

import uk.co.real_logic.sbe.ir.Token;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * SBE compositeType.
 */
public class CompositeType extends Type
{
    private final List<EncodedDataType> compositeList = new ArrayList<>();
    private final Map<String, EncodedDataType> compositeMap = new HashMap<>();
    private final int sinceVersion;

    /**
     * Construct a new compositeType from XML Schema.
     *
     * @param node from the XML Schema Parsing
     * @throws XPathExpressionException if the XPath is invalid.
     */
    public CompositeType(final Node node)
        throws XPathExpressionException
    {
        super(node);

        sinceVersion = Integer.parseInt(XmlSchemaParser.getAttributeValue(node, "sinceVersion", "0"));
        final XPath xPath = XPathFactory.newInstance().newXPath();
        final NodeList list = (NodeList)xPath.compile("type").evaluate(node, XPathConstants.NODESET);

        for (int i = 0, size = list.getLength(); i < size; i++)
        {
            final EncodedDataType type = new EncodedDataType(list.item(i));

            if (compositeMap.get(type.name()) != null)
            {
                XmlSchemaParser.handleError(node, "composite already has type name: " + type.name());
            }
            else
            {
                compositeList.add(type);
                compositeMap.put(type.name(), type);
            }
        }
    }

    /**
     * Return the EncodedDataType within this composite with the given name
     *
     * @param name of the EncodedDataType to return
     * @return type requested
     */
    public EncodedDataType getType(final String name)
    {
        return compositeMap.get(name);
    }

    /**
     * The size (in octets) of the list of EncodedDataTypes
     *
     * @return size of the compositeType
     */
    public int size()
    {
        int size = 0;

        for (final EncodedDataType t : compositeList)
        {
            if (t.isVariableLength())
            {
                return Token.VARIABLE_SIZE;
            }

            size += t.size();
        }

        return size;
    }

    /**
     * Return the sinceVersion value of the {@link CompositeType}
     *
     * @return the sinceVersion of the {@link CompositeType}
     */
    public int sinceVersion()
    {
        return sinceVersion;
    }

    /**
     * Return list of the EncodedDataTypes that compose this composite
     *
     * @return {@link List} that holds the types in this composite
     */
    public List<EncodedDataType> getTypeList()
    {
        return compositeList;
    }

    /**
     * Make this composite type, if it has a varData member, variable length
     * by making the EncodedDataType with the name "varData" be variable length.
     */
    public void makeDataFieldCompositeType()
    {
        final EncodedDataType edt = compositeMap.get("varData");
        if (edt != null)
        {
            edt.variableLength(true);
        }
    }

    /**
     * Check the composite for being a well formed group size encoding. This means
     * that there are the fields "blockLength" and "numInGroup" present.
     *
     * @param node of the XML for this composite
     */
    public void checkForWellFormedGroupSizeEncoding(final Node node)
    {
        if (compositeMap.get("blockLength") == null)
        {
            XmlSchemaParser.handleError(node, "composite for group size encoding must have \"blockLength\"");
        }

        if (compositeMap.get("numInGroup") == null)
        {
            XmlSchemaParser.handleError(node, "composite for group size encoding must have \"numInGroup\"");
        }
    }

    /**
     * Check the composite for being a well formed variable length data encoding. This means
     * that there are the fields "length" and "varData" present.
     *
     * @param node of the XML for this composite
     */
    public void checkForWellFormedVariableLengthDataEncoding(final Node node)
    {
        if (compositeMap.get("length") == null)
        {
            XmlSchemaParser.handleError(node, "composite for variable length data encoding must have \"length\"");
        }

        if (compositeMap.get("varData") == null)
        {
            XmlSchemaParser.handleError(node, "composite for variable length data encoding must have \"varData\"");
        }
    }

    /**
     * Check the composite for being a well formed message headerStructure encoding. This means
     * that there are the fields "blockLength", "templateId" and "version" present.
     *
     * @param node of the XML for this composite
     */
    public void checkForWellFormedMessageHeader(final Node node)
    {
        if (compositeMap.get("blockLength") == null)
        {
            XmlSchemaParser.handleError(node, "composite for message header must have \"blockLength\"");
        }

        if (compositeMap.get("templateId") == null)
        {
            XmlSchemaParser.handleError(node, "composite for message header must have \"templateId\"");
        }

        if (compositeMap.get("version") == null)
        {
            XmlSchemaParser.handleError(node, "composite for message header must have \"version\"");
        }
    }
}
