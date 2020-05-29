/*
 * Copyright (c) 2020. Free to copy and distribute
 */

package org.jcopybook;

import org.jcopybook.CopyBookComp;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.math.BigInteger;

public class CopyBookCompTest extends Assert {

    @Before
    public void init() throws Exception {

    }

    @Test
    public void stringToComp() throws Exception {
        // field1 PIC S9(04) COMP.  //Binary
        // field1 PIC 9(04) COMP.  //Binary
        String[] testValues = {"-1234", "1234", "+1234", "1234", "+1234"};
        int[] testPicSize = {4, 4, 4, 4, 4};
        boolean[] signed = {true, true, true, false, false};
        // only for hex view, actual bytes to be written to output file is from comp.getBytes()
        String[] testResults = {"fb2e", "04d2", "04d2", "04d2", "04d2"};
        for (int i = 0; i < testPicSize.length; i++) {
            CopyBookComp comp = new CopyBookComp(testValues[i], testPicSize[i], signed[i]);
            byte[] r = comp.getBytes();  // use bytes to set field for copybook or write to file

            //hex for test only
            String hex = comp.toDisplayString();
            System.out.println(testValues[i] + " ==> " + hex);
            assertEquals(testResults[i], hex);
        }

    }

    @Test
    public void bigIntegerToComp() throws Exception {
        String[] testValues = {"-1234", "1234", "+1234", "0000001234", "2"};
        int[] testPicSize = {4, 4, 4, 4, 4};
        boolean[] signed = {true, true, true, false, false};
        // only for hex view, actual bytes to be written to output file is from comp.getBytes()
        String[] testResults = {"fb2e", "04d2", "04d2", "04d2", "0002"};

        for (int i = 0; i < testPicSize.length; i++) {
            CopyBookComp comp = new CopyBookComp(new BigInteger(testValues[i]), testPicSize[i], signed[i]);
            byte[] r = comp.getBytes();  // use bytes to set field for copybook or write to file

            //hex for test only
            String hex = comp.toDisplayString();
            System.out.println(testValues[i] + " ==> " + hex);
            assertEquals(testResults[i], hex);
        }

    }

}
