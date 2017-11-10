package gabfeed2;

import com.cyberpointllc.stac.host.Main;

import java.io.File;

public class ServerManager {

    public static Main makeServer(int port, String dataPath, boolean rebuildDB,
                                  String serverKeyPath, String passswordKeyPath, String loginId) throws Exception {

        File dataPathFile = new File(dataPath);
        if (!dataPathFile.exists() || !dataPathFile.isDirectory()) {
            System.err.println("ERROR: specified datapath " + dataPath + " does not exist or is not a directory");
            System.exit(1);
        }

        if (serverKeyPath == null) {
            System.err.println("ERROR: a private key must be specified");
        }

        File serverKeyFile = new File(serverKeyPath);
        if (!serverKeyFile.exists()) {
            System.err.println("ERROR: specified private key " + serverKeyPath + " does not exist");
        }

        if (passswordKeyPath == null) {
            System.err.println("ERROR: a password key must be specified");
            System.exit(1);
        }

        File passwordKeyFile = new File(passswordKeyPath);
        if (!passwordKeyFile.exists()) {
            mainHelper2(passwordKeyFile);
        }

        return new Main(port, dataPath, rebuildDB, serverKeyFile, passwordKeyFile, loginId);
    }

    private static void mainHelper2(File passwordKeyFile) throws Exception {
        System.err.println("ERROR: specified password key " + passwordKeyFile + " does not exist");
        System.exit(1);
    }

}
