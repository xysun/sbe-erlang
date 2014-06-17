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
package uk.co.real_logic.sbe.examples;

import baselinesimple.*;
import uk.co.real_logic.sbe.codec.java.DirectBuffer;

import java.io.FileOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class ExampleUsingGeneratedStubSimple
{
    private static final String ENCODING_FILENAME = "sbe.encoding.filename";
    private static final byte[] MAKE;
    private static final byte[] MODEL;
    private static final byte[] VEHICLE_CODE;

    private static final MessageHeader MESSAGE_HEADER = new MessageHeader();
    private static final Car CAR = new Car();

    static
    {
        try
        {
            VEHICLE_CODE = "abcdef".getBytes(Car.vehicleCodeCharacterEncoding());
            MAKE = "Honda".getBytes(Car.makeCharacterEncoding());
            MODEL = "Civic VTi".getBytes(Car.modelCharacterEncoding());
        }
        catch (final UnsupportedEncodingException ex)
        {
            throw new RuntimeException(ex);
        }
    }

    public static void main(final String[] args)throws Exception
    {
        System.out.println("\n*** Basic Stub Example ***");

        final ByteBuffer byteBuffer = ByteBuffer.allocateDirect(4096);
        final DirectBuffer directBuffer = new DirectBuffer(byteBuffer);
        final short messageTemplateVersion = 0;
        int bufferOffset = 0;
        int encodingLength = 0;

        // Setup for encoding a message

        MESSAGE_HEADER.wrap(directBuffer, bufferOffset, messageTemplateVersion)
                      .blockLength(CAR.sbeBlockLength())
                      .templateId(CAR.sbeTemplateId())
                      .schemaId(CAR.sbeSchemaId())
                      .version(CAR.sbeSchemaVersion());

        bufferOffset += MESSAGE_HEADER.size();
        encodingLength += MESSAGE_HEADER.size();
        encodingLength += encode(CAR, directBuffer, bufferOffset);

        // Optionally write the encoded buffer to a file for decoding by the On-The-Fly decoder

        //final String encodingFilename = System.getProperty(ENCODING_FILENAME);
        final String encodingFilename = "car_java";
        if (encodingFilename != null)
        {
            try (final FileChannel channel = new FileOutputStream(encodingFilename).getChannel())
            {
                byteBuffer.limit(encodingLength);
                channel.write(byteBuffer);
            }
        }

        // Decode the encoded message
        // NOTE: read from erlang file, replace ByteBuffer, directBuffer
        File aFile = new File("car_erlang");
        FileInputStream inFile = new FileInputStream(aFile);
        FileChannel inChannel = inFile.getChannel();
        final ByteBuffer newBuffer = ByteBuffer.allocateDirect(4096);
        inChannel.read(newBuffer);

        final DirectBuffer decodeBuffer = new DirectBuffer(newBuffer);

        bufferOffset = 0;
        MESSAGE_HEADER.wrap(decodeBuffer, bufferOffset, messageTemplateVersion);

        // Lookup the applicable flyweight to decode this type of message based on templateId and version.
        final int templateId = MESSAGE_HEADER.templateId();
        if (templateId != baselinesimple.Car.TEMPLATE_ID)
        {
            throw new IllegalStateException("Template ids do not match");
        }

        final int actingBlockLength = MESSAGE_HEADER.blockLength();
        final int schemaId = MESSAGE_HEADER.schemaId();
        final int actingVersion = MESSAGE_HEADER.version();

        bufferOffset += MESSAGE_HEADER.size();
        decode(CAR, decodeBuffer, bufferOffset, actingBlockLength, schemaId, actingVersion);
    }

    public static int encode(final Car car, final DirectBuffer directBuffer, final int bufferOffset)
    {
        final int srcOffset = 0;

        car.wrapForEncode(directBuffer, bufferOffset)
           .modelYear(2023)
           .serialNumber(1234);
           //.modelYear(2013);
           //.putVehicleCode(VEHICLE_CODE, srcOffset);


        car.putMake(MAKE, srcOffset, MAKE.length);
       
        
        for (int i = 0, size = Car.someNumbersLength(); i < size; i++)
        {
            car.someNumbers(i, i);
        }

        car.putVehicleCode(VEHICLE_CODE, srcOffset);

        //car.putMake(MAKE, srcOffset, MAKE.length);
        car.putModel(MODEL, srcOffset, MODEL.length);

        return car.size();
    }

    public static void decode(final Car car,
                              final DirectBuffer directBuffer,
                              final int bufferOffset,
                              final int actingBlockLength,
                              final int schemaId,
                              final int actingVersion)
        throws Exception
    {
        final byte[] buffer = new byte[128];
        final StringBuilder sb = new StringBuilder();

        car.wrapForDecode(directBuffer, bufferOffset, actingBlockLength, actingVersion);

        sb.append("\ncar.templateId=").append(car.sbeTemplateId());
        sb.append("\ncar.schemaId=").append(schemaId);
        sb.append("\ncar.schemaVersion=").append(car.sbeSchemaVersion());
        sb.append("\ncar.serialNumber=").append(car.serialNumber());
        sb.append("\ncar.modelYear=").append(car.modelYear());

        sb.append("\ncar.someNumbers=");
        for (int i = 0, size = Car.someNumbersLength(); i < size; i++)
        {
            sb.append(car.someNumbers(i)).append(", ");
        }

        sb.append("\ncar.vehicleCode=");
        for (int i = 0, size = Car.vehicleCodeLength(); i < size; i++)
        {
            sb.append((char)car.vehicleCode(i));
        }

        sb.append("\ncar.make.semanticType=").append(Car.makeMetaAttribute(MetaAttribute.SEMANTIC_TYPE));
        sb.append("\ncar.make=").append(new String(buffer, 0, car.getMake(buffer, 0, buffer.length), Car.makeCharacterEncoding()));

        sb.append("\ncar.model=").append(new String(buffer, 0, car.getModel(buffer, 0, buffer.length), Car.modelCharacterEncoding()));

        sb.append("\ncar.size=").append(car.size());

        System.out.println(sb);
    }
}
