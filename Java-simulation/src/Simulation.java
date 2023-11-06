import java.util.*;
import java.util.stream.Collectors;

public class Simulation {
    private final char[] opinions;

    public Simulation(int numofAgents, double probability) {
        opinions = new char[numofAgents];
        for (int i = 0; i < opinions.length; i++) {
            opinions[i] = Math.random() < probability ? 'A' : 'B';
        }
    }

    public void simulateTwoChoice() {
        long startTime = System.currentTimeMillis();
        while (noConsensus(opinions)) {
            List<Integer> ints = randomNums(3);
            if (opinions[ints.get(1)] == opinions[ints.get(2)]) {
                opinions[ints.get(0)] = opinions[ints.get(1)];

            }

        }
        long time = System.currentTimeMillis() - startTime;
        System.out.printf("The Two choice simulation with %d agents took %d%n ms", opinions.length, time);
    }

    public void simulateThreeMajority() {
        long startTime = System.currentTimeMillis();
        while (noConsensus(opinions)) {
            List<Integer> ints = randomNums(4);
            if (opinions[ints.get(2)] == opinions[ints.get(3)]) {
                opinions[ints.get(0)] = opinions[ints.get(2)];
            } else {
                opinions[ints.get(0)] = opinions[ints.get(1)];
            }

        }
        long time = System.currentTimeMillis() - startTime;
        System.out.printf("The Three Majority simulation with %d agents took %d%n ms", opinions.length, time);
    }

    public void simulateUndecidedStateDynamics() {
        long startTime = System.currentTimeMillis();

        while (noConsensus(opinions)) {
            List<Integer> integers = randomNums(2);
            if (opinions[integers.get(0)] != opinions[integers.get(1)] && opinions[integers.get(1)] != 'U' && opinions[integers.get(0)] != 'U') {
                opinions[integers.get(0)] = 'U';
            } else if (opinions[integers.get(0)] == 'U' && opinions[integers.get(1)] != 'U') {
                opinions[integers.get(0)] = opinions[integers.get(1)];
            }
        }
        long time = System.currentTimeMillis() - startTime;

        System.out.printf("The Undecided State Dynamics simulation with %d agents took %d%n ms", opinions.length, time);
    }

    private List<Integer> randomNums(int n) {

        return new Random().ints(0, opinions.length).distinct().limit(n).boxed().collect(Collectors.toList());
    }

    private boolean noConsensus(char[] array) {
        for (char c : array) {
            if (c != array[0]) {
                return true;
            }
        }
        return false;
    }

}
