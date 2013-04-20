/* This file was generated by SableCC (http://www.sablecc.org/). */
package net.sf.cb2xml.sablecc.node;

import net.sf.cb2xml.sablecc.analysis.*;

import java.util.*;


public final class ARightLeftOrRight extends PLeftOrRight
{
    private TRight _right_;

    public ARightLeftOrRight ()
    {
    }

    public ARightLeftOrRight (TRight _right_)
    {
        setRight (_right_);
    }

    public Object clone ()
    {
        return new ARightLeftOrRight((TRight) cloneNode (_right_));
    }

    public void apply (Switch sw)
    {
        ((Analysis) sw).caseARightLeftOrRight (this);
    }

    public TRight getRight ()
    {
        return _right_;
    }

    public void setRight (TRight node)
    {
        if (_right_ != null)
        {
            _right_.parent (null);
        }

        if (node != null)
        {
            if (node.parent () != null)
            {
                node.parent ().removeChild (node);
            }

            node.parent (this);
        }

        _right_ = node;
    }

    public String toString ()
    {
        return "" + toString (_right_);
    }

    void removeChild (Node child)
    {
        if (_right_ == child)
        {
            _right_ = null;

            return;
        }
    }

    void replaceChild (Node oldChild, Node newChild)
    {
        if (_right_ == oldChild)
        {
            setRight ((TRight) newChild);

            return;
        }
    }
}