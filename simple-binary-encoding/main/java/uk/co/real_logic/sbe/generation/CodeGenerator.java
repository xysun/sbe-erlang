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
package uk.co.real_logic.sbe.generation;

import java.io.IOException;

/**
 * Abstraction for code generators to implement.
 */
public interface CodeGenerator
{
    /** Class name to be used for visitor pattern that accesses the message headerStructure. */
    String MESSAGE_HEADER_TYPE = "MessageHeader";

    /**
     * Generate the complete set of types and messages for a schema.
     *
     * @throws IOException if an error is encountered when writing the output.
     */
    void generate() throws IOException;
}
