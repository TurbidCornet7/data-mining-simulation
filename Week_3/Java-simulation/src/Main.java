import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("how many agents do you want");
        int numOfAgents = scanner.nextInt();
        System.out.println("Which simulation you wanna run 1, 2 or 3");
        int choice = scanner.nextInt();
        scanner.close();

        Simulation sim = new Simulation(numOfAgents, 0.5);
        switch (choice) {
            case 1 -> sim.simulateTwoChoice();
            case 2 -> sim.simulateThreeMajority();
            case 3 -> sim.simulateUndecidedStateDynamics();
            default -> System.out.println("Bad Choice");
        }

    }

}


