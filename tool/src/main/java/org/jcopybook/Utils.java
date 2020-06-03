package org.jcopybook;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.file.Files;
import java.util.Arrays;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class Utils {
	public static Element getFirstElement(Element documentElement) {
		Object node = null;
		NodeList childs = documentElement.getChildNodes();
		for (int i = 0; i < childs.getLength(); i++) {
			node = childs.item(i);
			if (node instanceof Element)
				return (Element) node;
		}
		throw new RuntimeException("Can't find any Element");
	}

	/**
	 * Iterative fix null value in text node
	 * 
	 * @param node
	 */
	public static void fixStringNode(Node node) {
		if (node.getNodeType() == Node.TEXT_NODE && node.getNodeValue() == null)
			node.setNodeValue("");
		NodeList childs = node.getChildNodes();
		for (int i = 0; i < childs.getLength(); i++) {
			fixStringNode(childs.item(i));
		}
	}

	public static byte[] convertToBCD(String val) {
		try {
			val = new String(val.getBytes("Cp1047"));
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		BigInteger v = new BigInteger(val.replaceAll("\\.", ""));
		byte[] bcd = new BigInteger(v.abs().toString(), 16).shiftLeft(4)
				.add(BigInteger.valueOf(v.signum() < 0 ? 13L : 12L))
				.toByteArray();
		byte[] b1 = Arrays.copyOfRange(bcd, 1, bcd.length);
		return bcd[0] == 0 ? b1 : bcd;
	}

	public static byte[] convertToBCD(String val, int size) {
		byte[] b = convertToBCD(val);
		if (b.length == size) {
			return b;
		}
		if (size < b.length) {
			throw new RuntimeException("Too big Comp-3 input data provided: "
					+ val + ". max size: " + size + ", provided: " + b.length);
		}

		byte[] bcd = new byte[size];
		Arrays.fill(bcd, (byte) 0);
		System.arraycopy(b, 0, bcd, size - b.length, b.length);
		return bcd;
	}

	public static byte[] convertToBinary(String val, int size, ByteOrder endian) {
		byte[] b = null;
		ByteBuffer bf = ByteBuffer.allocate(size);
		bf.order(endian);
		String s = val.replaceAll("\\.", "");
		if (size == Short.SIZE / 8 && Short.parseShort(s) <= Short.MAX_VALUE) {
			bf.putShort(Short.parseShort(s));
		} else if (size == Integer.SIZE / 8
				&& Integer.parseInt(s) <= Integer.MAX_VALUE) {
			bf.putInt(Integer.parseInt(s));
		} else if (size == Long.SIZE / 8 && Long.parseLong(s) <= Long.MAX_VALUE) {
			bf.putLong(Long.parseLong(s));
		} else {
			throw new RuntimeException("Invalid input data provided: " + val
					+ ". max size: " + size + ", provided: " + val.length());
		}

		b = bf.array();
		return b;
	}


	/*

https://stackoverflow.com/questions/38046275/write-a-file-on-mainframe-server-with-ebcdic-and-packed-decimal-format

package org.bkpradhan.jrecord;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

import java.io.IOException;

public class FieldNamesDtar020 {

    public static void main(String[] args) throws IOException {
        IIOBuilder iob = FieldNamesDtar020.newIoBuilder();
        AbstractLineWriter writer = iob.newWriter("/tmp/out_mf.txt");
        AbstractLine line = iob.newLine();


        line.getFieldValue(RECORD_DTAR020.keycodeNo).set("1235678");
        line.getFieldValue(RECORD_DTAR020.storeNo).set(111);
        line.getFieldValue(RECORD_DTAR020.date).set(222);
        line.getFieldValue(RECORD_DTAR020.deptNo).set(222);
        line.getFieldValue(RECORD_DTAR020.qtySold).set(1234);
        line.getFieldValue(RECORD_DTAR020.salePrice).set(1234.1234);
        System.out.println(line.getFieldValue(RECORD_DTAR020.qtySold).asHex());
        System.out.println(line.getFieldValue(RECORD_DTAR020.salePrice).asHex());
        byte[] value = line.getData();
        writer.write(line);
        writer.close();
    }

    public static final RecordDtar020 RECORD_DTAR020 = new RecordDtar020();



    public static IFixedWidthIOBuilder newIoBuilder() {
        RecordDtar020 r = RECORD_DTAR020;
        return JRecordInterface1.FIXED_WIDTH.newIOBuilder()
                .setFont("CP037")
                .setFileOrganization(Constants.IO_FIXED_LENGTH)
                .defineFieldsByLength()
                .addFieldByLength(r.keycodeNo, Type.ftChar, 8, 0)
                .addFieldByLength(r.storeNo, Type.ftPackedDecimal, 2, 0)
                .addFieldByLength(r.date, Type.ftPackedDecimal, 4, 0)
                .addFieldByLength(r.deptNo, Type.ftPackedDecimal, 2, 0)
                .addFieldByLength(r.qtySold, Type.ftBinaryBigEndian, 4, 0)
                .addFieldByLength(r.salePrice, Type.ftPackedDecimal, 6, 2)
                .endOfRecord();
    }

    public static class RecordDtar020 {
        public final String keycodeNo = "KEYCODE-NO";
        public final String storeNo = "STORE-NO";
        public final String date = "DATE";
        public final String deptNo = "DEPT-NO";
        public final String qtySold = "QTY-SOLD";
        public final String salePrice = "SALE-PRICE";

    }
}

	 */
}
