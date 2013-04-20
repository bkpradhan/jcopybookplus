/* This file was generated by SableCC (http://www.sablecc.org/). */
package net.sf.cb2xml.sablecc.node;

import net.sf.cb2xml.sablecc.analysis.*;

import java.util.*;


public final class AZerosLiteral extends PLiteral
{
    private TZeros _zeros_;

    public AZerosLiteral ()
    {
    }

    public AZerosLiteral (TZeros _zeros_)
    {
        setZeros (_zeros_);
    }

    public Object clone ()
    {
        return new AZerosLiteral((TZeros) cloneNode (_zeros_));
    }

    public void apply (Switch sw)
    {
        ((Analysis) sw).caseAZerosLiteral (this);
    }

    public TZeros getZeros ()
    {
        return _zeros_;
    }

    public void setZeros (TZeros node)
    {
        if (_zeros_ != null)
        {
            _zeros_.parent (null);
        }

        if (node != null)
        {
            if (node.parent () != null)
            {
                node.parent ().removeChild (node);
            }

            node.parent (this);
        }

        _zeros_ = node;
    }

    public String toString ()
    {
        return "" + toString (_zeros_);
    }

    void removeChild (Node child)
    {
        if (_zeros_ == child)
        {
            _zeros_ = null;

            return;
        }
    }

    void replaceChild (Node oldChild, Node newChild)
    {
        if (_zeros_ == oldChild)
        {
            setZeros ((TZeros) newChild);

            return;
        }
    }
}
