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
#ifndef _SBE_CAR_CODEC_BENCH_HPP
#define _SBE_CAR_CODEC_BENCH_HPP

#include "CodecBench.hpp"
#include "uk_co_real_logic_sbe_examples_car/Car.hpp"

using namespace uk_co_real_logic_sbe_examples_car;

char VEHICLE_CODE[] = {'a', 'b', 'c', 'd', 'e', 'f'};
uint32_t SOMENUMBERS[] = { 1, 2, 3, 4, 5 };
char MANUFACTURER_CODE[] = {'1', '2', '3'};
const char *MAKE = "Honda";
int MAKELEN = strlen(MAKE);
const char *MODEL = "Civic VTi";
int MODELLEN = strlen(MODEL);

class SbeCarCodecBench : public CodecBench<SbeCarCodecBench>
{
public:
    int encode(char *buffer, const int bufferLength)
    {
        car.wrapForEncode(buffer, 0, bufferLength)
           .serialNumber(1234)
           .modelYear(2013)
           .available(BooleanType::YES)
           .code(Model::A)
           .putVehicleCode(VEHICLE_CODE)
           .putSomeNumbers((char *)SOMENUMBERS);

        car.extras().clear()
           .cruiseControl(true)
           .sportsPack(true)
           .sunRoof(false);

        car.engine()
           .capacity(2000)
           .numCylinders((short)4)
           .putManufacturerCode(MANUFACTURER_CODE);

        car.fuelFiguresCount(3)
           .next().speed(30).mpg(35.9f)
           .next().speed(55).mpg(49.0f)
           .next().speed(75).mpg(40.0f);

        Car::PerformanceFigures &performanceFigures = car.performanceFiguresCount(2);

        performanceFigures.next()
            .octaneRating((short)95)
            .accelerationCount(3)
                .next().mph(30).seconds(4.0f)
                .next().mph(60).seconds(7.5f)
                .next().mph(100).seconds(12.2f);

        performanceFigures.next()
            .octaneRating((short)99)
            .accelerationCount(3)
                .next().mph(30).seconds(3.8f)
                .next().mph(60).seconds(7.1f)
                .next().mph(100).seconds(11.8f);

        car.putMake(MAKE, MAKELEN);
        car.putModel(MODEL, MODELLEN);

        return car.size();
    };

    virtual int decode(const char *buffer, const int bufferLength)
    {
        car.wrapForDecode((char *)buffer, 0, Car::sbeBlockLength(), Car::sbeSchemaVersion(), bufferLength);

        int64_t tmpInt;
        const char *tmpChar;
        double tmpDouble;
        bool tmpBool;

        tmpInt = car.serialNumber();
        tmpInt = car.modelYear();
        tmpInt = car.available();
        tmpInt = car.code();
        tmpChar = car.vehicleCode();
        tmpChar = car.someNumbers();

        OptionalExtras &extras = car.extras();
        tmpBool = extras.cruiseControl();
        tmpBool = extras.sportsPack();
        tmpBool = extras.sunRoof();

        Engine &engine = car.engine();
        tmpInt = engine.capacity();
        tmpInt = engine.numCylinders();
        tmpInt = engine.maxRpm();
        tmpChar = engine.manufacturerCode();
        tmpChar = engine.fuel();

        Car::FuelFigures &fuelFigures = car.fuelFigures();
        while (fuelFigures.hasNext())
        {
            fuelFigures.next();
            tmpInt = fuelFigures.speed();
            tmpDouble = fuelFigures.mpg();
        }

        Car::PerformanceFigures &performanceFigures = car.performanceFigures();
        while (performanceFigures.hasNext())
        {
            performanceFigures.next();
            tmpInt = performanceFigures.octaneRating();

            Car::PerformanceFigures::Acceleration &acceleration = performanceFigures.acceleration();
            while (acceleration.hasNext())
            {
                acceleration.next();
                tmpInt = acceleration.mph();
                tmpDouble = acceleration.seconds();
            }
        }

        tmpChar = car.make();
        tmpChar = car.model();

        return car.size();
    };

private:
    Car car;
};

#endif /* _SBE_CAR_CODEC_BENCH_HPP */
