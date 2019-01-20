
import java.util.Scanner;

public class Menu {
    private Scanner input;
    public Menu(){
        this.input = new Scanner(System.in);
        input.useDelimiter("[\r,\n]");
    }

    public String readString(String question){
        System.out.println(question);
        return  input.next();
    }

    public int show(String[] entries) {
        int option = 0;

        String menu = String.join("\n", entries);
        System.out.println(menu + "\n");

        while(option <= 0 || option > entries.length) {
            option = readInt("Choose one of the options: ");
            if (option <= 0 || option > entries.length)
                System.out.println("\n> Invalid\n");
        }

        return option;
    }

    public void printResponse(String response) {
        if (response.length() > 0)
            response += "\n";

        System.out.print("\n" + response);
    }


    public Boolean readBoolean(String question) {
        Boolean mode;
        try {
            System.out.println(question);
            mode = Boolean.parseBoolean(input.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Invalid ! Please input valid number");
            mode = readBoolean(question);
        }

        return mode;
    }


    public Integer readInt(String question) {
        int mode;
        try {
            System.out.println(question);
            mode = Integer.parseInt(input.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Invalid ! Please input valid number");
            mode =readInt(question);
        }
        return mode;
    }

    public Float readFloat(String question) {
        float mode;
        try {
            System.out.println(question);
            mode = Float.parseFloat(input.nextLine());
        } catch (NumberFormatException e) {
            System.out.println("Invalid ! Please input valid number");
            mode =readInt(question);
        }
        return mode;
    }


}
