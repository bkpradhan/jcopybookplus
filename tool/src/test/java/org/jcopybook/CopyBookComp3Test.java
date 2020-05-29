/*
 * Copyright (c) 2020. Free to copy and distribute
 */

package org.jcopybook;

import org.jcopybook.CopyBookComp3;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

public class CopyBookComp3Test extends Assert {

    @Before
    public void init() throws Exception {

    }

    @Test
    public void stringToComp3() throws Exception {
        //              Value  Comp-3, hex
        //                +0           0C
        //                +1           1C
        //                +12        01 2C
        //                +123        12 3C
        //                +1234     01 23 4C
        //                -1           1D
        //                -1234     01 23 4D

        // field2 PIC S9(5) COMP-3.
        //        -12345 ==> 12345d
        //        12345 ==> 12345c

        // field3 PIC 9(08)V9(07) COMP-3.
        //        12345678.7654321 ==> 123456787654321f
        //        12345.7654 ==> 000000123457654f


        // field4 PIC 9(08)V9(07) COMP-3.
        //        12345678.76 ==> 000001234567876f
        //        123456.76543 ==> 000012345676543f


        // field5 PIC 9(07) COMP-3.
        //        1234567 ==> 1234567f

        // field6 PIC S9(10)V9(3) COMP-3.
        //        1234567890.123 ==> 1234567890123c
        //                -12345.12 ==> 0000001234512d

        // field7 PIC S9(10)V9(3) COMP-3.
        //                +1234567890.123 ==> 1234567890123c
        //                -1234567890.1 ==> 0012345678901d

        // field8 PIC S9(9) COMP-3.
        //       -12345 ==> 000012345d
        //        12345 ==> 000012345c

        String[] testValues = {"-12345", "12345", "8.2700000", "33.87", "23.8181000", "23.1000000", "296.5000000", "12345678.123", "-12345.12", "+12345678.123", "-12345678.1", "-12345", "12345"};
        int[] testDigitsBefore = {5, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9};
        int[] testDigitsAfter = {0, 0, 7, 7, 7, 7, 7, 5, 5, 5, 5, 0, 0};
        boolean[] signed = {true, true, false, false, false, false, false, true, true, true, true, true, true};
        // only for hex view, actual bytes to be written to output file is from comp3.getBytes()
        String[] testResults = {"12345d", "12345c", "000000082700000f", "000000338700000f", "000000238181000f", "000000231000000f", "000002965000000f", "1234567812300c", "0001234512000d", "1234567812300c", "1234567810000d", "000012345d", "000012345c"};

        for (int i = 0; i < testDigitsBefore.length; i++) {
            CopyBookComp3 comp3 = new CopyBookComp3(testValues[i], testDigitsBefore[i], testDigitsAfter[i], signed[i]);
            byte[] r = comp3.getBytes();  // use bytes to set field for copybook or write to file

            //hex for test only
            String hex = comp3.toDisplayString();
            System.out.println(testValues[i] + " ==> " + hex);
            assertEquals(testResults[i], hex );
        }

    }

    @Test
    public void bigDecimalToComp3() throws Exception {
        String[] testValues = {"-12345", "12345", "8.2700000", "33.87", "23.8181000", "23.1000000", "296.5000000", "12345678.123", "-12345.12", "+12345678.123", "-12345678.1", "-12345", "12345", ".00078", "-12345.0"};
        int[] testDigitsBefore = {5, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 0, 6};
        int[] testDigitsAfter = {0, 0, 7, 7, 7, 7, 7, 5, 5, 5, 5, 0, 0, 8, 0};
        boolean[] signed = {true, true, false, false, false, false, false, true, true, true, true, true, true, true, true};
        // only for hex view, actual bytes to be written to output file is from comp3.getBytes()
        String[] testResults = {"12345d", "12345c", "000000082700000f", "000000338700000f", "000000238181000f", "000000231000000f", "000002965000000f", "1234567812300c", "0001234512000d", "1234567812300c", "1234567810000d", "000012345d", "000012345c", "000078000c", "0012345d"};

        for (int i = 0; i < testDigitsBefore.length; i++) {
            CopyBookComp3 comp3 = new CopyBookComp3(new BigDecimal(testValues[i]), testDigitsBefore[i], testDigitsAfter[i], signed[i]);
            byte[] r = comp3.getBytes();  // use bytes to set field for copybook or write to file

            //hex for test only
            String hex = comp3.toDisplayString();
            System.out.println(testValues[i] + " ==> " + hex);
            assertEquals(testResults[i], hex);
        }

    }

    @Test
    public void bigIntegerToComp3() throws Exception {
        String[] testValues = {"-12345", "12345", "+123456787654321", "-00000000000000000123457654000000", "+1234567876", "+1234560"};
        int[] testDigitsBefore = {5, 5, 8, 8, 8, 8};
        int[] testDigitsAfter = {0, 0, 7, 7, 7, 7};
        boolean[] signed = {true, true, false, true, false, true};
        // only for hex view, actual bytes to be written to output file is from comp3.getBytes()
        String[] testResults = {"12345d", "12345c", "123456787654321f", "123457654000000d", "000001234567876f", "000000001234560c"};

        for (int i = 0; i < testDigitsBefore.length; i++) {
            CopyBookComp3 comp3 = new CopyBookComp3(new BigInteger(testValues[i]), testDigitsBefore[i]+testDigitsAfter[i], signed[i]);
            byte[] r = comp3.getBytes();  // use bytes to set field for copybook or write to file

            //hex for test only
            String hex = comp3.toDisplayString();
            System.out.println(testValues[i] + " ==> " + hex);
            assertEquals(testResults[i], hex);
        }

    }

}
