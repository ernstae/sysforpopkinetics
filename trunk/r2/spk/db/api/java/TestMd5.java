import rfpk.spk.spkdb.*;
import java.sql.*;

public class TestMd5 {
    public static void main(String args[]) {
	String password = args[0];
	String sum = Spkdb.md5sum(password);
	System.out.println(sum);
    }
}
