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

import extension.Car;
import uk.co.real_logic.sbe.codec.java.DirectBuffer;

import java.io.FileOutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class ExampleUsingGeneratedStubExtension
{
    private static final String ENCODING_FILENAME = "sbe.encoding.filename";
    private static final byte[] VEHICLE_CODE;
    private static final byte[] MANUFACTURER_CODE;
    private static final byte[] MAKE;
    private static final byte[] MODEL;

    private static final baseline.MessageHeader MESSAGE_HEADER = new baseline.MessageHeader();
    private static final baseline.Car CAR_0 = new baseline.Car();
    private static final extension.Car CAR_1 = new extension.Car();

    static
    {
        try
        {
            VEHICLE_CODE = "abcdef".getBytes(baseline.Car.vehicleCodeCharacterEncoding());
            MANUFACTURER_CODE = "123".getBytes(baseline.Engine.manufacturerCodeCharacterEncoding());
            MAKE = "Honda".getBytes(baseline.Car.makeCharacterEncoding());
            MODEL = "Civic VTi".getBytes(baseline.Car.modelCharacterEncoding());
        }
        catch (final UnsupportedEncodingException ex)
        {
            throw new RuntimeException(ex);
        }
    }

    public static void main(final String[] args)throws Exception
    {
        System.out.println("\n*** Extension Stub Example ***");

        final ByteBuffer byteBuffer = ByteBuffer.allocateDirect(4096);
        final DirectBuffer directBuffer = new DirectBuffer(byteBuffer);
        final short messageTemplateVersion = 0;
        int bufferOffset = 0;
        int encodingLength = 0;

        // Setup for encoding a message

        MESSAGE_HEADER.wrap(directBuffer, bufferOffset, messageTemplateVersion)
                      .blockLength(CAR_0.sbeBlockLength())
                      .templateId(CAR_0.sbeTemplateId())
                      .schemaId(CAR_0.sbeSchemaId())
                      .version(CAR_0.sbeSchemaVersion());

        bufferOffset += MESSAGE_HEADER.size();
        encodingLength += MESSAGE_HEADER.size();
        encodingLength += encode(CAR_0, directBuffer, bufferOffset);

        // Optionally write the encoded buffer to a file for decoding by the On-The-Fly decoder

        final String encodingFilename = System.getProperty(ENCODING_FILENAME);
        if (encodingFilename != null)
        {
            try (final FileChannel channel = new FileOutputStream(encodingFilename).getChannel())
            {
                byteBuffer.limit(encodingLength);
                channel.write(byteBuffer);
            }
        }

        // Decode the encoded message

        bufferOffset = 0;
        MESSAGE_HEADER.wrap(directBuffer, bufferOffset, messageTemplateVersion);

        // Lookup the applicable flyweight to decode this type of message based on templateId and version.
        final int templateId = MESSAGE_HEADER.templateId();
        if (templateId != extension.Car.TEMPLATE_ID)
        {
            throw new IllegalStateException("Template ids do not match");
        }

        final int actingBlockLength = MESSAGE_HEADER.blockLength();
        final int schemaId = MESSAGE_HEADER.schemaId();
        final int actingVersion = MESSAGE_HEADER.version();

        bufferOffset += MESSAGE_HEADER.size();
        decode(CAR_1, directBuffer, bufferOffset, schemaId, actingBlockLength, actingVersion);
    }

    public static int encode(final baseline.Car car,
                             final DirectBuffer directBuffer,
                             final int bufferOffset)
    {
        final int srcOffset = 0;

        car.wrapForEncode(directBuffer, bufferOffset)
           .serialNumber(1234)
           .modelYear(2013)
           .available(baseline.BooleanType.TRUE)
           .code(baseline.Model.A)
           .putVehicleCode(VEHICLE_CODE, srcOffset);

        for (int i = 0, size = baseline.Car.someNumbersLength(); i < size; i++)
        {
            car.someNumbers(i, i);
        }

        car.extras().clear()
                    .cruiseControl(true)
                    .sportsPack(true)
                    .sunRoof(false);

        car.engine().capacity(2000)
                    .numCylinders((short)4)
                    .putManufacturerCode(MANUFACTURER_CODE, srcOffset);

        car.fuelFiguresCount(3).next().speed(30).mpg(35.9f)
                               .next().speed(55).mpg(49.0f)
                               .next().speed(75).mpg(40.0f);

        final baseline.Car.PerformanceFigures perfFigures = car.performanceFiguresCount(2);
        perfFigures.next().octaneRating((short)95)
                          .accelerationCount(3).next().mph(30).seconds(4.0f)
                                               .next().mph(60).seconds(7.5f)
                                               .next().mph(100).seconds(12.2f);
        perfFigures.next().octaneRating((short)99)
                          .accelerationCount(3).next().mph(30).seconds(3.8f)
                                               .next().mph(60).seconds(7.1f)
                                               .next().mph(100).seconds(11.8f);

        car.putMake(MAKE, srcOffset, MAKE.length);
        car.putModel(MODEL, srcOffset, MODEL.length);

        return car.size();
    }

    public static void decode(final extension.Car car,
                              final DirectBuffer directBuffer,
                              final int bufferOffset,
                              final int schemaId,
                              final int actingBlockLength,
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
        sb.append("\ncar.available=").append(car.available());
        sb.append("\ncar.code=").append(car.code());

        sb.append("\ncar.someNumbers=");
        for (int i = 0, size = extension.Car.someNumbersLength(); i < size; i++)
        {
            sb.append(car.someNumbers(i)).append(", ");
        }

        sb.append("\ncar.vehicleCode=");
        for (int i = 0, size = extension.Car.vehicleCodeLength(); i < size; i++)
        {
            sb.append((char)car.vehicleCode(i));
        }

        final extension.OptionalExtras extras = car.extras();
        sb.append("\ncar.extras.cruiseControl=").append(extras.cruiseControl());
        sb.append("\ncar.extras.sportsPack=").append(extras.sportsPack());
        sb.append("\ncar.extras.sunRoof=").append(extras.sunRoof());

        final extension.Engine engine = car.engine();
        sb.append("\ncar.engine.capacity=").append(engine.capacity());
        sb.append("\ncar.engine.numCylinders=").append(engine.numCylinders());
        sb.append("\ncar.engine.maxRpm=").append(engine.maxRpm());
        sb.append("\ncar.engine.manufacturerCode=");
        for (int i = 0, size = extension.Engine.manufacturerCodeLength(); i < size; i++)
        {
            sb.append((char)engine.manufacturerCode(i));
        }

        sb.append("\ncar.engine.fuel=").append(new String(buffer, 0, engine.getFuel(buffer, 0, buffer.length), "ASCII"));

        final String cupHolderCount = car.cupHolderCount() == Car.cupHolderCountNullValue() ? "null" : car.cupHolderCount() + "";
        sb.append("\ncar.cutHolderCount=").append(cupHolderCount);

        for (final extension.Car.FuelFigures fuelFigures : car.fuelFigures())
        {
            sb.append("\ncar.fuelFigures.speed=").append(fuelFigures.speed());
            sb.append("\ncar.fuelFigures.mpg=").append(fuelFigures.mpg());
        }

        for (final extension.Car.PerformanceFigures performanceFigures : car.performanceFigures())
        {
            sb.append("\ncar.performanceFigures.octaneRating=").append(performanceFigures.octaneRating());

            for (final extension.Car.PerformanceFigures.Acceleration acceleration : performanceFigures.acceleration())
            {
                sb.append("\ncar.performanceFigures.acceleration.mph=").append(acceleration.mph());
                sb.append("\ncar.performanceFigures.acceleration.seconds=").append(acceleration.seconds());
            }
        }

        sb.append("\ncar.make.semanticType=").append(extension.Car.makeMetaAttribute(extension.MetaAttribute.SEMANTIC_TYPE));
        sb.append("\ncar.make=").append(new String(buffer, 0, car.getMake(buffer, 0, buffer.length), extension.Car.makeCharacterEncoding()));

        sb.append("\ncar.model=").append(new String(buffer, 0, car.getModel(buffer, 0, buffer.length), extension.Car.modelCharacterEncoding()));

        sb.append("\ncar.size=").append(car.size());

        System.out.println(sb);
    }
}
