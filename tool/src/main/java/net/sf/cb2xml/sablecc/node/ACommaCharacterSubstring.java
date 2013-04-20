/* This file was generated by SableCC (http://www.sablecc.org/). */
package net.sf.cb2xml.sablecc.node;

import net.sf.cb2xml.sablecc.analysis.*;

import java.util.*;


public final class ACommaCharacterSubstring extends PCharacterSubstring
{
    private TComma _comma_;

    public ACommaCharacterSubstring ()
    {
    }

    public ACommaCharacterSubstring (TComma _comma_)
    {
        setComma (_comma_);
    }

    public Object clone ()
    {
        return new ACommaCharacterSubstring((TComma) cloneNode (_comma_));
    }

    public void apply (Switch sw)
    {
        ((Analysis) sw).caseACommaCharacterSubstring (this);
    }

    public TComma getComma ()
    {
        return _comma_;
    }

    public void setComma (TComma node)
    {
        if (_comma_ != null)
        {
            _comma_.parent (null);
        }

        if (node != null)
        {
            if (node.parent () != null)
            {
                node.parent ().removeChild (node);
            }

            node.parent (this);
        }

        _comma_ = node;
    }

    public String toString ()
    {
        return "" + toString (_comma_);
    }

    void removeChild (Node child)
    {
        if (_comma_ == child)
        {
            _comma_ = null;

            return;
        }
    }

    void replaceChild (Node oldChild, Node newChild)
    {
        if (_comma_ == oldChild)
        {
            setComma ((TComma) newChild);

            return;
        }
    }
}
