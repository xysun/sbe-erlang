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
#include "benchlet.hpp"
#include "PbMarketDataCodecBench.hpp"

#define MAX_MD_BUFFER (1000*1000)

class PbMarketDataBench : public Benchmark
{
public:
    virtual void setUp(void)
    {
        buffer_ = new char[MAX_MD_BUFFER];
        bench_.runEncode(buffer_);  // set buffer up for decoding runs
    };

    virtual void tearDown(void)
    {
        delete[] buffer_;
    };

    PbMarketDataCodecBench bench_;
    char *buffer_;
};

static struct Benchmark::Config cfg[] = {
    { Benchmark::ITERATIONS, "1000000" },
    { Benchmark::BATCHES, "20" }
};

BENCHMARK_CONFIG(PbMarketDataBench, RunSingleEncode, cfg)
{
    bench_.runEncode(buffer_);
}

BENCHMARK_CONFIG(PbMarketDataBench, RunSingleDecode, cfg)
{
    bench_.runDecode(buffer_);
}

BENCHMARK_CONFIG(PbMarketDataBench, RunSingleEncodeAndDecode, cfg)
{
    bench_.runEncodeAndDecode(buffer_);
}
