import java.security.*;
import java.io.*;
import java.util.Arrays;

public class Regression {

    static int longHash(String s) throws NoSuchAlgorithmException {
	MessageDigest md = MessageDigest.getInstance("MD5");

	md.update(s.getBytes());
	byte[] h = md.digest();

	return (h[0] << 24) | (h[1] & 0xff) << 16 | (h[2] & 0xff) << 8 | (h[3] & 0xff);
    }

    // This function has a buffer containing a valid Protocol Compiler
    // message. It submits every subset that starts at offset zero to the
    // unmarshaller. They should all throw exceptions.

    static void testInvalid() throws IOException, Exception {
	{
	    byte[] packet = new byte[] { 0x53, 0x44, 0x44, 0x02, 0x51, 0x03, 0x14, 0x09, (byte) 0x8f, 0x6b,
		    (byte) 0xcd, 0x12, 0x2c, (byte) 0xba, 0x51, 0x0a, 0x12, (byte) 0x93, (byte) 0x8a, 0x12,
		    (byte) 0x92, 0x62, 0x12, (byte) 0xa6, (byte) 0xc3, 0x14, 0x08, 0x04, (byte) 0xb1, (byte) 0x80,
		    0x12, 0x68, (byte) 0x99, 0x18, 0x00, (byte) 0xbe, 0x4b, (byte) 0x80, (byte) 0xbf, (byte) 0xea,
		    (byte) 0xce, 0x28, 0x12, (byte) 0x9f, (byte) 0xa0, 0x28, 0x00, (byte) 0xbb, 0x0c, (byte) 0xa0,
		    0x08, 0x04, (byte) 0xc0, 0x00, 0x12, (byte) 0xbb, (byte) 0xd3, 0x41, 0x00 };

	    {
		ByteArrayInputStream is = new ByteArrayInputStream(packet);
		test.Request.unmarshal(is);
	    }

	    for (int ii = 0; ii < packet.length - 1; ++ii) {
		ByteArrayInputStream is = new ByteArrayInputStream(packet, 0, ii);

		try {
		    test.Request.unmarshal(is);
		} catch (Exception e) {
		    continue;
		}
		throw new Exception("accepted invalid message when length was " + ii);
	    }
	}

	{
	    byte[] packet = new byte[] { 0x53, 0x44, 0x44, 0x02, 0x51, 0x03, 0x14, 0x09, (byte) 0x8f, 0x6b,
		    (byte) 0xcd, 0x12, 0x2c, (byte) 0xba, 0x51, 0x08, 0x12, (byte) 0xa6, (byte) 0xc3, 0x14, 0x08, 0x04,
		    (byte) 0xb1, (byte) 0x80, 0x12, 0x68, (byte) 0x99, 0x18, 0x00, (byte) 0xbe, 0x4b, (byte) 0x80,
		    (byte) 0xbf, (byte) 0xea, (byte) 0xce, 0x28, 0x12, (byte) 0x9f, (byte) 0xa0, 0x28, 0x00,
		    (byte) 0xbb, 0x0c, (byte) 0xa0, 0x08, 0x04, (byte) 0xc0, 0x00, 0x12, (byte) 0xbb, (byte) 0xd3,
		    0x41, 0x00 };
	    ByteArrayInputStream is = new ByteArrayInputStream(packet);

	    try {
		test.Request.unmarshal(is);
	    } catch (Exception e) {
		return;
	    }
	    throw new Exception("accepted invalid message when missing required field");
	}
    }

    // A helper function that places an SDD header into the stream.

    static void emitHeader(OutputStream os) throws IOException {
	os.write(new byte[] { 0x53, 0x44, 0x44, 0x02, 0x51, 0x03 });
    }

    // A helper function that computes a 32-bit hash value and places into
    // the stream.

    static void emitHash32(OutputStream os, String str) throws NoSuchAlgorithmException, IOException {
	int h = longHash(str);

	os.write(0x14);
	os.write(h >> 24);
	os.write(h >> 16);
	os.write(h >> 8);
	os.write(h);
    }

    // A helper function that computes a 16-bit hash value and places into
    // the stream.

    static void emitHash16(OutputStream os, String str) throws NoSuchAlgorithmException, IOException {
	int h = longHash(str);

	os.write(0x12);
	os.write(h >> 8);
	os.write(h);
    }

    static void testReq32(int skipField) throws NoSuchAlgorithmException, IOException {
	ByteArrayOutputStream os = new ByteArrayOutputStream();

	emitHeader(os);
	emitHash32(os, "test");
	emitHash16(os, "reqReq32");
	os.write(0x51);
	os.write(skipField < 32 ? 62 : 64);

	for (int jj = 0; jj < 32; ++jj)
	    if (skipField != jj) {
		emitHash16(os, "fld" + ((jj + 1) / 10) + ((jj + 1) % 10));
		os.write(0x12);
		os.write(0);
		os.write(0);
	    }

	ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());

	test.Request.unmarshal(is);
    }

    static void testReq33(int skipField) throws NoSuchAlgorithmException, IOException {
	ByteArrayOutputStream os = new ByteArrayOutputStream();

	emitHeader(os);
	emitHash32(os, "test");
	emitHash16(os, "reqReq33");
	os.write(0x51);
	os.write(skipField < 33 ? 64 : 66);

	for (int jj = 0; jj < 33; ++jj)
	    if (skipField != jj) {
		emitHash16(os, "fld" + ((jj + 1) / 10) + ((jj + 1) % 10));
		os.write(0x12);
		os.write(0);
		os.write(0);
	    }

	ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());

	test.Request.unmarshal(is);
    }

    static void testMissingRequiredFields() throws NoSuchAlgorithmException, IOException, Exception {
	testReq32(Integer.MAX_VALUE);
	for (int ii = 0; ii < 32; ++ii) {
	    try {
		testReq32(ii);
	    } catch (IOException e) {
		continue;
	    }
	    throw new Exception("decoded Req32 structure with missing #" + ii + " field");
	}
	testReq33(Integer.MAX_VALUE);
	for (int ii = 0; ii < 33; ++ii) {
	    try {
		testReq33(ii);
	    } catch (IOException e) {
		continue;
	    }
	    throw new Exception("decoded Req33 structure with missing #" + ii + " field");
	}
    }

    static void _test() throws IOException, Exception {
	test.Reply.Test tmp = new test.Reply.Test();
	ByteArrayOutputStream os = new ByteArrayOutputStream();

	tmp.marshal(os);
	ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());

	test.Reply.unmarshal(is);
	try {
	    test.Request.unmarshal(is);
	} catch (Exception e) {
	    return;
	}
	throw new Exception("request unmarshaller decoded Test reply");
    }

    // This function tests the separation of the messages (that requests
    // can't get confused with replies.) The test.proto file has a request
    // call Test and a reply called Test. This function encodes a Test
    // request and makes sure it can only be decoded by the request
    // unmarshaller. The same is done with the reply version.

    static void testSeparation() throws IOException, Exception {
	// Test requests should only be decoable by the Test request
	// unmarshaller.

	{
	    test.Request.Test tmp = new test.Request.Test();
	    ByteArrayOutputStream os = new ByteArrayOutputStream();

	    tmp.marshal(os);
	    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());

	    test.Request.unmarshal(is);
	    try {
		test.Reply.unmarshal(is);
	    } catch (IOException e) {
		_test();
		return;
	    }
	    throw new Exception("reply unmarshaller decoded Test request");
	}

	// Test replies should only be decoable by the Test reply
	// unmarshaller.

    }

    static void test() throws Exception {
	test.TEMP tmp = new test.TEMP();
	test.Request.Container2 c2 = new test.Request.Container2();
	test.Primitives prim = new test.Primitives();

	ByteArrayOutputStream os = new ByteArrayOutputStream();
	c2.marshal(os);

	tmp.f6 = "a";

	System.out.println(tmp);
	System.out.println(prim);
    }

    static int marshalLength(test msg) throws java.io.IOException {
	ByteArrayOutputStream os = new ByteArrayOutputStream();
	msg.marshal(os);
	return os.size();	
    }

    // This class inherits from the Receiver interface so it can easily
    // decode messages.

    static class Tester implements test.Request.Receiver {
	public test.Request.Message msg = new test.Request.Message();
	public test.Request.Container con1 = new test.Request.Container();
	public test.Request.Container2 con2 = new test.Request.Container2();

	Tester() {
	}

	public void handle(test.Request.Message tmp) {
	    if (tmp.equals(msg))
		return;
	    throw new RuntimeException("receive message doesn't match sent message");
	}

	public void handle(test.Request.Container tmp) {
	    if (tmp.equals(con1))
		return;
	    throw new RuntimeException("receive message doesn't match sent message (con1)");
	}

	public void handle(test.Request.EnumRequest tmp) {
	}

	public void handle(test.Request.Container2 tmp) {
	    if (tmp.equals(con2))
		return;
	    throw new RuntimeException("receive message doesn't match sent message (con2)");
	}

	public void handle(test.Request.Test tmp) {
	    throw new RuntimeException("unexpected Test message");
	}

	public void handle(test.Request.Req32 tmp) {
	    throw new RuntimeException("unexpected Req32 message");
	}

	public void handle(test.Request.Req33 tmp) {
	    throw new RuntimeException("unexpected Req33 message");
	}

	public void handle(test.Request.Arr tmp) {
	    throw new RuntimeException("unexpected Arr message");
	}

	public void handle(test.Request.PrimitiveArray tmp) {
	    throw new RuntimeException("unexpected PrimitiveArray message");
	}
    };

    static void dumpBytes(byte[] b)
    {
	for (int ii = 0; ii < b.length; ii++)
	    System.out.printf("0x%02x %c\n", b[ii], Character.isLetterOrDigit(b[ii]) ? b[ii] : ' ');
    }

    static void sendMessage(Tester t) throws IOException {
	{
	    ByteArrayOutputStream os = new ByteArrayOutputStream();

	    t.msg.shInt = 0x1234;
	    t.msg.Int = 0x56789abc;
	    t.msg.lgInt = 0xfedcba9876543210l;
	    t.msg.Dbl = 1.5;
	    t.msg.Str = "Testing";

	    t.msg.marshal(os);

	    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());

	    test.Request ptr = test.Request.unmarshal(is);

	    ptr.deliverTo(t);
	}

	{
	    t.con1.data.msg = "hello";
	    t.con2.data.msg = "goodbye";

	    ByteArrayOutputStream os = new ByteArrayOutputStream();
	    t.con1.marshal(os);

	    ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
	    test.Request ptr = test.Request.unmarshal(is);

	    ptr.deliverTo(t);

	    t.con2.data.msg = "hello";
	    t.con1.data.msg = "goodbye";

	    try {
		ptr.deliverTo(t);
	    } catch (Exception e) {
		return;
	    }
	    throw new IOException("delivery confused two similar messages");
	}
    }

    public static void main(String[] args) {
	try {
	    Tester tmp = new Tester();

	    testInvalid();
	    testMissingRequiredFields();
	    sendMessage(tmp);
	    testSeparation();

	    // Enum testing

	    try {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		test.Request.EnumRequest r = new test.Request.EnumRequest();
		r.e = test.SomeEnums.One;
		r.marshal(os);
	    } catch (Exception e) {
		System.out.println("Failed marshalling enum message");
		throw e;
	    }
	    
	    try {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		test.Request.EnumRequest r = new test.Request.EnumRequest();
		r.marshal(os);
		throw new IOException("should have thrown a NullPointerException for uninitialized enum");	
	    } catch (NullPointerException e) {
	    }

	    try {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		test.Request.EnumRequest r = new test.Request.EnumRequest();
		r.e = test.SomeEnums.One;
		r.marshal(os);

		ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
		test.Request ur = test.Request.unmarshal(is);
	    } catch (Exception e) {
		System.out.println("Error unmarshalling EnumRequest");
		e.printStackTrace();
	    }

	    try {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		test.Request.EnumRequest r = new test.Request.EnumRequest();
		r.e = test.SomeEnums.One;
		r.marshal(os);

	    } catch (Exception e) {
		System.out.println("Error unmarshalling EnumRequest");
		e.printStackTrace();
	    }
	    //test();

	    System.out.println("All tests passed successfully!");
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
