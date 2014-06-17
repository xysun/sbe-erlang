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

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import uk.co.real_logic.sbe.PrimitiveType;
import uk.co.real_logic.sbe.PrimitiveValue;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import static uk.co.real_logic.sbe.xml.XmlSchemaParser.handleError;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.handleWarning;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.checkForValidName;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.getAttributeValue;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.getAttributeValueOrNull;

/**
 * SBE enumType
 */
public class EnumType extends Type
{
    private final PrimitiveType encodingType;
    private final PrimitiveValue nullValue;
    private final Map<PrimitiveValue, ValidValue> validValueByPrimitiveValueMap = new LinkedHashMap<>();
    private final Map<String, ValidValue> validValueByNameMap = new LinkedHashMap<>();

    /**
     * Construct a new enumType from XML Schema.
     *
     * @param node from the XML Schema Parsing
     * @throws XPathExpressionException if the XPath is invalid
     */
    public EnumType(final Node node)
        throws XPathExpressionException
    {
        super(node);

        final XPath xPath = XPathFactory.newInstance().newXPath();
        final String encodingTypeStr = getAttributeValue(node, "encodingType");
        final EncodedDataType encodedDataType;

        switch (encodingTypeStr)
        {
            case "char":
            case "uint8":
            case "int8":
            case "int16":
            case "uint16":
            case "int32":
                encodingType = PrimitiveType.get(encodingTypeStr);
                encodedDataType = null;
                break;

            default:
                // might not have ran into this type yet, so look for it
                final Node encodingTypeNode =
                    (Node)xPath.compile(String.format("%s[@name=\'%s\']", XmlSchemaParser.TYPE_XPATH_EXPR, encodingTypeStr))
                    .evaluate(node.getOwnerDocument(), XPathConstants.NODE);

                if (null == encodingTypeNode)
                {
                    throw new IllegalArgumentException("illegal encodingType for enum " + encodingTypeStr);
                }

                encodedDataType = new EncodedDataType(encodingTypeNode);

                if (encodedDataType.length() != 1)
                {
                    throw new IllegalArgumentException("illegal encodingType for enum " + encodingTypeStr + " length not equal to 1");
                }

                encodingType = encodedDataType.primitiveType();
        }

        final String nullValueStr = getAttributeValueOrNull(node, "nullValue");
        if (null != nullValueStr)
        {
            if (presence() != Presence.OPTIONAL)
            {
                handleError(node, "nullValue set, but presence is not optional");
            }

            nullValue = PrimitiveValue.parse(nullValueStr, encodingType);
        }
        else if (presence() == Presence.OPTIONAL)
        {
            if (null != encodedDataType && null != encodedDataType.nullValue())
            {
                nullValue = encodedDataType.nullValue();
            }
            else
            {
                handleError(node, "presence optional but no null value found");
                nullValue = null;
            }
        }
        else
        {
            nullValue = null;
        }

        final NodeList list = (NodeList)xPath.compile("validValue").evaluate(node, XPathConstants.NODESET);

        for (int i = 0, size = list.getLength(); i < size; i++)
        {
            final ValidValue v = new ValidValue(list.item(i), encodingType);

            if (validValueByPrimitiveValueMap.get(v.primitiveValue()) != null)
            {
                handleWarning(node, "validValue already exists for value: " + v.primitiveValue());
            }

            if (validValueByNameMap.get(v.name()) != null)
            {
                handleWarning(node, "validValue already exists for name: " + v.name());
            }

            validValueByPrimitiveValueMap.put(v.primitiveValue(), v);
            validValueByNameMap.put(v.name(), v);
        }
    }

    /**
     * The {@link PrimitiveType} used to encode the enum.
     *
     * @return the {@link PrimitiveType} used to encode the enum.
     */
    public PrimitiveType encodingType()
    {
        return encodingType;
    }

    /**
     * The size (in octets) of the encodingType
     *
     * @return size of the encodingType
     */
    public int size()
    {
        return encodingType.size();
    }

    /**
     * Get the {@link ValidValue} represented by a {@link PrimitiveValue}.
     *
     * @param value to lookup
     * @return the {@link ValidValue} represented by a {@link PrimitiveValue} or null.
     */
    public ValidValue getValidValue(final PrimitiveValue value)
    {
        return validValueByPrimitiveValueMap.get(value);
    }

    /**
     * Get the {@link ValidValue} represented by a string name.
     *
     * @param name to lookup
     * @return the {@link ValidValue} represented by a string name or null.
     */
    public ValidValue getValidValue(final String name)
    {
        return validValueByNameMap.get(name);
    }

    /**
     * The nullValue of the type
     *
     * @return value of the nullValue
     */
    public PrimitiveValue nullValue()
    {
        return nullValue;
    }

    /**
     * The collection of valid values for this enumeration.
     *
     * @return the collection of valid values for this enumeration.
     */
    public Collection<ValidValue> validValues()
    {
        return validValueByNameMap.values();
    }

    /**
     * Class to hold valid values for EnumType
     */
    public static class ValidValue
    {
        private final String name;
        private final String description;
        private final PrimitiveValue value;
        private final int sinceVersion;

        /**
         * Construct a ValidValue given the XML node and the encodingType.
         *
         * @param node         that contains the validValue
         * @param encodingType for the enum
         */
        public ValidValue(final Node node, final PrimitiveType encodingType)
        {
            name = getAttributeValue(node, "name");
            description = getAttributeValueOrNull(node, "description");
            value = PrimitiveValue.parse(node.getFirstChild().getNodeValue(), encodingType);
            sinceVersion = Integer.parseInt(getAttributeValue(node, "sinceVersion", "0"));

            checkForValidName(node, name);
        }

        /**
         * {@link PrimitiveType} for the {@link ValidValue}.
         *
         * @return {@link PrimitiveType} for the {@link ValidValue}.
         */
        public PrimitiveValue primitiveValue()
        {
            return value;
        }

        /**
         * The name of the {@link ValidValue}.
         *
         * @return the name of the {@link ValidValue}
         */
        public String name()
        {
            return name;
        }

        /**
         * The description of the {@link ValidValue}.
         *
         * @return the description of the {@link ValidValue}.
         */
        public String description()
        {
            return description;
        }

        /**
         * The sinceVersion value of the {@link ValidValue}
         *
         * @return the sinceVersion value of the {@link ValidValue}
         */
        public int sinceVersion()
        {
            return sinceVersion;
        }
    }
}
