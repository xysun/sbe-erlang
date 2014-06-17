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
package uk.co.real_logic.sbe.otf;

import uk.co.real_logic.sbe.codec.java.DirectBuffer;
import uk.co.real_logic.sbe.ir.Token;

import java.util.List;

/**
 * Abstract {@link TokenListener} that can be extended when not all callback methods are required.
 *
 * By extending this class their is a possibility for the optimizer to elide unused methods otherwise requiring polymorphic dispatch.
 */
public abstract class AbstractTokenListener implements TokenListener
{
    public void onBeginMessage(final Token token)
    {
        // no op
    }

    public void onEndMessage(final Token token)
    {
        // no op
    }

    public void onEncoding(final Token fieldToken, final DirectBuffer buffer, final int bufferIndex, final Token typeToken, final int actingVersion)
    {
        // no op
    }

    public void onEnum(final Token fieldToken, final DirectBuffer buffer, final int bufferIndex,
                       final List<Token> tokens, final int fromIndex, final int toIndex, final int actingVersion)
    {
        // no op
    }

    public void onBitSet(final Token fieldToken, final DirectBuffer buffer,
                         final int bufferIndex, final List<Token> tokens, final int fromIndex, final int toIndex, final int actingVersion)
    {
        // no op
    }

    public void onBeginComposite(final Token fieldToken, final List<Token> tokens, final int fromIndex, final int toIndex)
    {
        // no op
    }

    public void onEndComposite(final Token fieldToken, final List<Token> tokens, final int fromIndex, final int toIndex)
    {
        // no op
    }

    public void onBeginGroup(final Token token, final int groupIndex, final int numInGroup)
    {
        // no op
    }

    public void onEndGroup(final Token token, final int groupIndex, final int numInGroup)
    {
        // no op
    }

    public void onVarData(final Token fieldToken, final DirectBuffer buffer, final int bufferIndex, final int length, final Token typeToken)
    {
        // no op
    }
}
