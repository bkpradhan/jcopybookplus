/* This file was generated by SableCC (http://www.sablecc.org/). */
package net.sf.cb2xml.sablecc.node;

import net.sf.cb2xml.sablecc.analysis.*;

import java.util.*;


public final class ASingleGroupItem extends PGroupItem
{
    private PElementaryItem _elementaryItem_;

    public ASingleGroupItem ()
    {
    }

    public ASingleGroupItem (PElementaryItem _elementaryItem_)
    {
        setElementaryItem (_elementaryItem_);
    }

    public Object clone ()
    {
        return new ASingleGroupItem((PElementaryItem) cloneNode (
                _elementaryItem_));
    }

    public void apply (Switch sw)
    {
        ((Analysis) sw).caseASingleGroupItem (this);
    }

    public PElementaryItem getElementaryItem ()
    {
        return _elementaryItem_;
    }

    public void setElementaryItem (PElementaryItem node)
    {
        if (_elementaryItem_ != null)
        {
            _elementaryItem_.parent (null);
        }

        if (node != null)
        {
            if (node.parent () != null)
            {
                node.parent ().removeChild (node);
            }

            node.parent (this);
        }

        _elementaryItem_ = node;
    }

    public String toString ()
    {
        return "" + toString (_elementaryItem_);
    }

    void removeChild (Node child)
    {
        if (_elementaryItem_ == child)
        {
            _elementaryItem_ = null;

            return;
        }
    }

    void replaceChild (Node oldChild, Node newChild)
    {
        if (_elementaryItem_ == oldChild)
        {
            setElementaryItem ((PElementaryItem) newChild);

            return;
        }
    }
}
