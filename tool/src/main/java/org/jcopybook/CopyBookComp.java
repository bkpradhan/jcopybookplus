/*
 * Copyright (c) 2020. Free to copy and distribute
 */

package org.jcopybook;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * User: bkpradhan
 * Date: 2020.05.25 23:38
 */

/**
 * Computational / comp / binary is Binary format encoding used mainly in Mainframe/Cobol data type layouts, though format is NOT cross platform as it is vendor dependent such as IBM, Micro Focus, etc
 * Byte Size and word size and endian-ness impact actual binary output, so comp datatype is not encouraged to be used in cross platform integration
 * literature: http://www.simotime.com/databn01.htm
 * Allows to create a Comp data type from a string, big integer format and getBytes provides Comp/binary output
 * toDisplayString([ByteOrder)] is for Hex display only.
 */

public class CopyBookComp extends CopyBookComp3 {
    public static final int SHORT_BUFF_SIZE = Short.SIZE / Byte.SIZE;
    public static final int INT_BUFF_SIZE = Integer.SIZE / Byte.SIZE;
    public static final int LONG_BUFF_SIZE = Long.SIZE / Byte.SIZE;

    private int pictureSize = 0;

    public static void main(String[] args) {
        CopyBookComp comp = new CopyBookComp("-123", 4, true);
        byte[] r = comp.getBytes();  // use bytes to set field for copybook or write to file

        //hex for test only
        System.out.println("-123" + " ==> " + comp.toDisplayString());
        System.out.println("-123" + " ==> " + comp.toDisplayString(ByteOrder.LITTLE_ENDIAN));

        comp = new CopyBookComp("123", 4, true);
        r = comp.getBytes();  // use bytes to set field for copybook or write to file

        //hex for test only
        System.out.println("123" + " ==> " + comp.toDisplayString());
        System.out.println("123" + " ==> " + comp.toDisplayString(ByteOrder.LITTLE_ENDIAN));
    }

    public CopyBookComp(String value, int pictureSize) {
        this(value, pictureSize, true);
    }

    public CopyBookComp(String value, int pictureSize, boolean signed) {
        super(value, pictureSize, signed);
        this.pictureSize = pictureSize;
    }

    public CopyBookComp(BigInteger value, int pictureSize, boolean signed) {
        super(value, pictureSize, signed);
        this.pictureSize = pictureSize;
    }

    public CopyBookComp(BigInteger value, int pictureSize) {
        this(value, pictureSize, true);
    }

    public byte[] getBytes() {
        result = convertToBinaryWithPicSize(String.valueOf(internalValue), pictureSize, ByteOrder.BIG_ENDIAN);
        return result;
    }

    public static byte[] convertToBinary(String val, int size, ByteOrder endian) {
        byte[] b = null;
        ByteBuffer bf = ByteBuffer.allocate(size);
        bf.order(endian);
        String s = val.replaceAll("\\.", "");
        if (size == SHORT_BUFF_SIZE && Short.parseShort(s) <= Short.MAX_VALUE) {
            bf.putShort(Short.parseShort(s));
        } else if (size == INT_BUFF_SIZE && Integer.parseInt(s) <= Integer.MAX_VALUE) {
            bf.putInt(Integer.parseInt(s));
        } else if (size == LONG_BUFF_SIZE && Long.parseLong(s) <= Long.MAX_VALUE) {
            bf.putLong(Long.parseLong(s));
        } else {
            throw new RuntimeException("Invalid input data provided: " + s
                    + ". max size: " + size + ", provided: " + s.length());
        }

        b = bf.array();
        return b;
    }

    public static byte[] convertToBinaryWithPicSize(String s, int pictureSize, ByteOrder endian) {
        int len = pictureSize < 5 ? SHORT_BUFF_SIZE : pictureSize < 10 ? INT_BUFF_SIZE : LONG_BUFF_SIZE;
        return convertToBinary(s, len, endian);
    }

}
