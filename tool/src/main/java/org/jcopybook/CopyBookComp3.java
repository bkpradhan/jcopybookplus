/*
 * Copyright (c) 2020. Free to copy and distribute
 */

package org.jcopybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteOrder;
import java.util.Arrays;

/**
 * User: bkpradhan
 * Date: 2020.05.25 22:28
 */

/**
 * Comp-3 / Packed Decimal is Binary Coded Decimal encoding used mainly in Mainframe/Cobol data type layouts, though format is cross platform
 * literature: http://www.3480-3590-data-conversion.com/article-packed-fields.html
 * Allows to create a Comp3 data type from a string, big integer or big decimal format and getBytes provides Comp3/BCD/Packed Decimal output
 * toDisplayString([ByteOrder)] is for Hex display only.
 */
public class CopyBookComp3 {
    protected BigInteger internalValue = null;
    private BigInteger signInLastNibble = null;
    private int packedSize = 0;
    private boolean isSigned = true;
    protected byte[] result = null;
    public static final String ZEROS_50="00000000000000000000000000000000000000000000000000";

    public static void main(String[] args) {

        CopyBookComp3 comp3 = new CopyBookComp3("-123", 4, 2, true);
        byte[] r = comp3.getBytes();  // use bytes to set field for copybook or write to file

        //hex for test only
        System.out.println("-123" + " ==> " + comp3.toDisplayString());
        System.out.println("-123" + " ==> " + comp3.toDisplayString(ByteOrder.LITTLE_ENDIAN));

        comp3 = new CopyBookComp3("8.27", 8, 2, false);
        r = comp3.getBytes();  // use bytes to set field for copybook or write to file

        //hex for test only
        System.out.println("8.2700000" + " ==> " + comp3.toDisplayString());
        System.out.println("8.2700000" + " ==> " + comp3.toDisplayString(ByteOrder.LITTLE_ENDIAN));
    }

    /**
     * pic s9(8) comp-3 with a value of 12345.678
     * comp31 = new CopyBookComp3("12345.678", 8);
     * defaults to signed pic
     *
     * @return
     */

    public CopyBookComp3(String value, int digitsBefore, int digitsAfter) {
        this(value, digitsBefore, digitsAfter, true);
    }

    /**
     * pic 9(8) comp-3 with a value of 12345678
     * comp32 = new CopyBookComp3("12345678", 8, false);
     *
     * @return
     */
    public CopyBookComp3(String value, int digitsBefore, int digitsAfter, boolean signed) {
        //String paddedValue = value.indexOf(".") > -1 ? rightPadZeros(value, digitsBefore + digitsAfter - value.indexOf(".")) : rightPadZeros(value, digitsAfter);
        this(new BigInteger(rightPadZeros(value, digitsBefore, digitsAfter)), digitsBefore + digitsAfter , signed);
    }

    /**
     * pic s9(8) comp-3 with a value of 12345678
     * comp33 = new CopyBookComp3(new BigInteger("12345678"), 8, true);
     *
     * @return
     */
    public CopyBookComp3(BigInteger value, int pictureSize, boolean signed) {
        internalValue = value;
        packedSize = (int) Math.ceil(0.5 * (pictureSize + 1)); // Calculate packed size
        isSigned = signed;
        // unsigned - F, negative - D, positive - C
        signInLastNibble = BigInteger.valueOf(!isSigned() ? 0xF : internalValue.signum() < 0 ? 0xD : 0xC);
    }

    /**
     * pic s9(8) comp-3 with a value of 12345678
     * comp33 = new CopyBookComp3(new BigInteger("12345678"), 8, true);
     * defaults to signed pic
     * @return
     */
    public CopyBookComp3(BigInteger value, int pictureSize) {
        this(value, pictureSize, true);

    }

    /**
     * pic s9(4)v9(4) comp-3 with a assumed value of 1234.5678
     * comp34 = new CopyBookComp3(new BigDecimal("1234.5678"), 8, true);
     * @return
     */
    public CopyBookComp3(BigDecimal value, int digitsBefore, int digitsAfter, boolean signed) {
        this(value.stripTrailingZeros().toPlainString(), digitsBefore, digitsAfter, signed);
    }

    /**
     * pic s9(4)v9(4) comp-3 with a assumed value of 1234.5678
     * comp35 = new CopyBookComp3(new BigDecimal("1234.5678"), 8);
     * defaults to signed pic
     * @return
     */
    public CopyBookComp3(BigDecimal value, int digitsBefore, int digitsAfter) {
        this(value, digitsBefore, digitsAfter, true);
    }

    public boolean isSigned() {
        return isSigned;
    }

    //convert to BCD  - Binary Coded Decimal - Comp3
    public byte[] convertToBCD(BigInteger fieldValue) {
        byte[] bcd = new BigInteger(fieldValue.abs().toString(), 16).shiftLeft(4)
                .add(signInLastNibble)
                .toByteArray();
        byte[] b1 = Arrays.copyOfRange(bcd, 1, bcd.length);
        return bcd[0] == 0 ? b1 : bcd;
    }

    public byte[] convertToBCD(BigInteger fieldValue, int size) {
        byte[] b = convertToBCD(fieldValue);
        if (b.length == size) {
            return b;
        }
        if (size < b.length) {
            throw new RuntimeException("Too big Comp-3 input data provided: "
                    + fieldValue + ". max size: " + size + ", provided: " + b.length);
        }

        byte[] bcd = new byte[size];
        Arrays.fill(bcd, (byte) 0);
        System.arraycopy(b, 0, bcd, size - b.length, b.length);
        return bcd;
    }

    public static String encodeToHex(byte[] byteArray, ByteOrder byteOrder) {
        StringBuilder sb = new StringBuilder(byteArray.length * 2);

        int index;
        char first4Bit;
        char last4Bit;
        for (int i = 0; i < byteArray.length; i++) {
            index = (byteOrder == ByteOrder.BIG_ENDIAN) ? i : byteArray.length - i - 1;
            first4Bit = Character.forDigit((byteArray[index] >> 4) & 0xF, 16);
            last4Bit = Character.forDigit((byteArray[index] & 0xF), 16);
            sb.append(first4Bit).append(last4Bit);
        }
        return sb.toString();
    }

    public byte[] getBytes() {
        result = convertToBCD(internalValue, packedSize);
        return result;
    }

    public String toDisplayString() {
        return toDisplayString(ByteOrder.BIG_ENDIAN);
    }

    public String toDisplayString(ByteOrder endian) {
        return result == null ? null : encodeToHex(result, endian);
    }

    public static String repeat(String symb, int num) {
        if ( num == 0 ){ return "" ; }
        StringBuilder buf = new StringBuilder();
        for (int i = 0; i < num; i++) {
            buf.append(symb);
        }
        return buf.toString();
    }

    public static String rightPadZeros(String value, int digitsBefore, int digitsAfter) {
        int start = value.indexOf(".");
        int end = value.length() - start -1;
        int pad = digitsAfter - end;
        String result = value.contains(".") ? (value+ZEROS_50.substring(0, pad)).replaceAll("\\.", "") : value.concat(new String(ZEROS_50.toCharArray(), 0, digitsAfter)); //+repeat("0", digitsAfter);
        return result;
    }
}
