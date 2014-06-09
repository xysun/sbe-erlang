/* Generated SBE (Simple Binary Encoding) message codec */
package uk.co.real_logic.sbe.ir.generated;

import uk.co.real_logic.sbe.codec.java.*;

public enum PresenceCodec
{
    SBE_REQUIRED((short)0),
    SBE_OPTIONAL((short)1),
    SBE_CONSTANT((short)2),
    NULL_VAL((short)255);

    private final short value;

    PresenceCodec(final short value)
    {
        this.value = value;
    }

    public short value()
    {
        return value;
    }

    public static PresenceCodec get(final short value)
    {
        switch (value)
        {
            case 0: return SBE_REQUIRED;
            case 1: return SBE_OPTIONAL;
            case 2: return SBE_CONSTANT;
        }

        if ((short)255 == value)
        {
            return NULL_VAL;
        }

        throw new IllegalArgumentException("Unknown value: " + value);
    }
}
