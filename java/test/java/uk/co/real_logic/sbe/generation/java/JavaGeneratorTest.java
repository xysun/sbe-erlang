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
package uk.co.real_logic.sbe.generation.java;

import org.junit.Before;
import org.junit.Test;
import uk.co.real_logic.sbe.SbeTool;
import uk.co.real_logic.sbe.TestUtil;
import uk.co.real_logic.sbe.codec.java.DirectBuffer;
import uk.co.real_logic.sbe.generation.java.util.CompilerUtil;
import uk.co.real_logic.sbe.generation.java.util.StringWriterOutputManager;
import uk.co.real_logic.sbe.ir.Ir;
import uk.co.real_logic.sbe.xml.IrGenerator;
import uk.co.real_logic.sbe.xml.MessageSchema;

import java.lang.reflect.Method;
import java.nio.ByteOrder;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.*;
import static uk.co.real_logic.sbe.generation.java.JavaGenerator.MESSAGE_HEADER_TYPE;
import static uk.co.real_logic.sbe.xml.XmlSchemaParser.parse;

public class JavaGeneratorTest
{
    private static final ByteOrder BYTE_ORDER = ByteOrder.nativeOrder();
    private final StringWriterOutputManager outputManager = new StringWriterOutputManager();
    private final DirectBuffer mockBuffer = mock(DirectBuffer.class);

    private Ir ir;

    @Before
    public void setUp() throws Exception
    {
        System.setProperty(SbeTool.VALIDATION_STOP_ON_ERROR, "true");

        final MessageSchema schema = parse(TestUtil.getLocalResource("code-generation-schema.xml"));
        final IrGenerator irg = new IrGenerator();
        ir = irg.generate(schema);

        outputManager.clear();
        outputManager.setPackageName(ir.applicableNamespace());
    }

    @Test
    public void shouldGenerateMessageHeaderStub() throws Exception
    {
        final int bufferOffset = 64;
        final int templateIdOffset = 2;
        final Short templateId = Short.valueOf((short)7);
        final int actingVersion = 0;
        final Integer blockLength = Integer.valueOf(32);
        final String fqClassName = ir.applicableNamespace() + "." + MESSAGE_HEADER_TYPE;

        when(Short.valueOf(mockBuffer.getShort(bufferOffset + templateIdOffset, BYTE_ORDER))).thenReturn(templateId);

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generateMessageHeaderStub();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object flyweight = clazz.newInstance();
        final Method method = flyweight.getClass().getDeclaredMethod("wrap", DirectBuffer.class, int.class, int.class);
        method.invoke(flyweight, mockBuffer, Integer.valueOf(bufferOffset), Integer.valueOf(actingVersion));

        final Integer result = (Integer)clazz.getDeclaredMethod("templateId").invoke(flyweight);
        assertThat(result, is(Integer.valueOf(templateId.intValue())));

        clazz.getDeclaredMethod("blockLength", int.class).invoke(flyweight, blockLength);

        verify(mockBuffer).putShort(bufferOffset, blockLength.shortValue(), BYTE_ORDER);
    }

    @Test
    public void shouldGenerateUint8EnumStub() throws Exception
    {
        final String className = "BooleanType";
        final String fqClassName = ir.applicableNamespace() + "." + className;

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generateTypeStubs();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object result = clazz.getDeclaredMethod("get", short.class).invoke(null, Short.valueOf((short)1));

        assertThat(result.toString(), is("TRUE"));
    }

    @Test
    public void shouldGenerateCharEnumStub() throws Exception
    {
        final String className = "Model";
        final String fqClassName = ir.applicableNamespace() + "." + className;

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generateTypeStubs();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object result = clazz.getDeclaredMethod("get", byte.class).invoke(null, Byte.valueOf((byte)'B'));

        assertThat(result.toString(), is("B"));
    }

    @Test
    public void shouldGenerateChoiceSetStub() throws Exception
    {
        final int bufferOffset = 8;
        final int actingVersion = 0;
        final Byte bitset = Byte.valueOf((byte)0b0000_0100);
        final String className = "OptionalExtras";
        final String fqClassName = ir.applicableNamespace() + "." + className;

        when(Byte.valueOf(mockBuffer.getByte(bufferOffset))).thenReturn(bitset);

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generateTypeStubs();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object flyweight = clazz.newInstance();
        final Method method = flyweight.getClass().getDeclaredMethod("wrap", DirectBuffer.class, int.class, int.class);
        method.invoke(flyweight, mockBuffer, Integer.valueOf(bufferOffset), Integer.valueOf(actingVersion));

        final Object result = clazz.getDeclaredMethod("cruiseControl").invoke(flyweight);

        assertThat((Boolean)result, is(Boolean.TRUE));
    }

    @Test
    public void shouldGenerateCompositeStub() throws Exception
    {
        final int actingVersion = 0;
        final int bufferOffset = 64;
        final int capacityFieldOffset = bufferOffset;
        final int numCylindersOffset = bufferOffset + 2;
        final int expectedEngineCapacity = 2000;
        final int expectedMaxRpm = 9000;
        final int manufacturerCodeOffset = bufferOffset + 3;
        final byte[] manufacturerCode = {'A', 'B', 'C'};
        final String className = "Engine";
        final String fqClassName = ir.applicableNamespace() + "." + className;

        when(Short.valueOf(mockBuffer.getShort(capacityFieldOffset, BYTE_ORDER))).thenReturn(Short.valueOf((short)expectedEngineCapacity));

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generateTypeStubs();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object flyweight = clazz.newInstance();
        final Method method = flyweight.getClass().getDeclaredMethod("wrap", DirectBuffer.class, int.class, int.class);
        method.invoke(flyweight, mockBuffer, Integer.valueOf(bufferOffset), Integer.valueOf(actingVersion));

        final Integer capacityResult = (Integer)clazz.getDeclaredMethod("capacity").invoke(flyweight);
        assertThat(capacityResult, is(Integer.valueOf(expectedEngineCapacity)));

        final Integer maxRpmResult = (Integer)clazz.getDeclaredMethod("maxRpm").invoke(flyweight);
        assertThat(maxRpmResult, is(Integer.valueOf(expectedMaxRpm)));

        final short numCylinders = (short)4;
        clazz.getDeclaredMethod("numCylinders", short.class).invoke(flyweight, Short.valueOf(numCylinders));

        clazz.getDeclaredMethod("putManufacturerCode", byte[].class, int.class)
             .invoke(flyweight, manufacturerCode, Integer.valueOf(0));

        verify(mockBuffer).putByte(numCylindersOffset, (byte)numCylinders);
        verify(mockBuffer).putBytes(manufacturerCodeOffset, manufacturerCode, 0, manufacturerCode.length);
    }

    @Test
    public void shouldGenerateBasicMessage() throws Exception
    {
        final DirectBuffer buffer = new DirectBuffer(new byte[4096]);
        final String className = "Car";
        final String fqClassName = ir.applicableNamespace() + "." + className;

        final JavaGenerator javaGenerator = new JavaGenerator(ir, outputManager);
        javaGenerator.generate();

        final Class<?> clazz = CompilerUtil.compileInMemory(fqClassName, outputManager.getSources());
        assertNotNull(clazz);

        final Object msgFlyweight = clazz.newInstance();
        msgFlyweight.getClass().getDeclaredMethod("wrapForEncode", DirectBuffer.class, int.class)
                               .invoke(msgFlyweight, mockBuffer, Integer.valueOf(0));

        final Integer initialPosition = (Integer)msgFlyweight.getClass().getDeclaredMethod("limit").invoke(msgFlyweight);

        final Object groupFlyweight = clazz.getDeclaredMethod("fuelFigures").invoke(msgFlyweight);
        assertThat((Integer)msgFlyweight.getClass().getDeclaredMethod("limit").invoke(msgFlyweight), greaterThan(initialPosition));

        final Integer count = (Integer)groupFlyweight.getClass().getDeclaredMethod("count").invoke(groupFlyweight);
        assertThat(count, is(Integer.valueOf(0)));
    }
}