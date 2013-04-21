package org.jcopybook;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
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
}
