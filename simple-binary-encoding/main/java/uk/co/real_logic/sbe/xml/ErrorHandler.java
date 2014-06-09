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

import java.io.PrintStream;

/**
 * Class to hold error handling state while parsing an XML message schema.
 */
public class ErrorHandler
{
    private final PrintStream out;
    private final boolean stopOnError;
    private final boolean warningsFatal;
    private final boolean suppressOutput;
    private int errors = 0;
    private int warnings = 0;

    /**
     * Construct a new {@link ErrorHandler} that outputs to a provided {@link PrintStream}.
     *
     * @param stream to which output should be sent.
     */
    public ErrorHandler(final PrintStream stream)
    {
        out = stream;
        stopOnError = Boolean.parseBoolean(System.getProperty(SbeTool.VALIDATION_STOP_ON_ERROR));
        warningsFatal = Boolean.parseBoolean(System.getProperty(SbeTool.VALIDATION_WARNINGS_FATAL));
        suppressOutput = Boolean.parseBoolean(System.getProperty(SbeTool.VALIDATION_SUPPRESS_OUTPUT));
    }

    /**
     * Default {@link ErrorHandler} that outputs to {@link System#err}.
     */
    public ErrorHandler()
    {
        this(System.err);
    }

    /**
     * Record a message signifying an error condition.
     *
     * @param msg signifying an error.
     */
    public void error(final String msg)
    {
        errors++;

        if (!suppressOutput)
        {
            out.println("ERROR: " + msg);
        }

        if (stopOnError)
        {
            throw new IllegalArgumentException(msg);
        }
    }

    /**
     * Record a message signifying an warning condition.
     *
     * @param msg signifying an warning.
     */
    public void warning(final String msg)
    {
        warnings++;

        if (!suppressOutput)
        {
            out.println("WARNING: " + msg);
        }

        if (warningsFatal && stopOnError)
        {
            throw new IllegalArgumentException(msg);
        }
    }

    /**
     * Check if the parser should exit.
     *
     * @throws IllegalStateException if there are errors or warnings recorded.
     */
    public void checkIfShouldExit()
    {
        if (errors > 0)
        {
            throw new IllegalStateException("had " + errors + " errors");
        }
        else if (warnings > 0 && warningsFatal)
        {
            throw new IllegalStateException("had " + warnings + " warnings");
        }
    }

    /**
     * The count of errors encountered.
     *
     * @return the count of errors encountered.
     */
    public int errorCount()
    {
        return errors;
    }

    /**
     * The count of warnings encountered.
     *
     * @return the count of warnings encountered.
     */
    public int warningCount()
    {
        return warnings;
    }

    public String toString()
    {
        return "ErrorHandler{" +
            "out=" + out +
            ", stopOnError=" + stopOnError +
            ", warningsFatal=" + warningsFatal +
            ", suppressOutput=" + suppressOutput +
            ", errors=" + errors +
            ", warnings=" + warnings +
            '}';
    }
}
