import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Simulation {
    private final char[] opinions;
    File csvOutputFile;
    public Simulation(int numofAgents, double probability) {
        opinions = new char[numofAgents];
        for (int i = 0; i < opinions.length; i++) {
            opinions[i] = Math.random() < probability ? 'A' : 'B';
        }
    }

    public void simulateTwoChoice() {
        long startTime = System.currentTimeMillis();
        int count = 0, index = 0;
        while (noConsensus(opinions)) {
            List<Integer> ints = randomNums(3);
            if (opinions[ints.get(1)] == opinions[ints.get(2)]) {
                opinions[ints.get(0)] = opinions[ints.get(1)];

            }
            ++count;
            if (count % (opinions.length/2) == 0) {
                printCSV(index, startTime, System.currentTimeMillis());
                index++;
            }//this part after how many iteration you print the status of the opinions
        }
        printCSV(index, startTime, System.currentTimeMillis());
        long time = System.currentTimeMillis() - startTime;
        System.out.printf("The Two choice simulation with %d agents took %d%n ms", opinions.length, time);
    }

    private void printCSV(int i, long startTime, long time) {
        csvOutputFile = new File("outputs/output" + i + ".csv");
        try (FileWriter writer = new FileWriter(csvOutputFile)) {
            String content = new String(opinions);
            writer.write("Time: " + (time - startTime) + "ms\n" );
            writer.write(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    public void simulateThreeMajority() {
        long startTime = System.currentTimeMillis();
        int count = 0, index = 0;
        while (noConsensus(opinions)) {
            List<Integer> ints = randomNums(4);
            if (opinions[ints.get(2)] == opinions[ints.get(3)]) {
                opinions[ints.get(0)] = opinions[ints.get(2)];
            } else {
                opinions[ints.get(0)] = opinions[ints.get(1)];
            }
            ++count;
            if (count % (opinions.length/2) == 0) {
                printCSV(index, startTime, System.currentTimeMillis());
                ++index;
            }

        }
        printCSV(index, startTime, System.currentTimeMillis());
        long time = System.currentTimeMillis() - startTime;
        System.out.printf("The Three Majority simulation with %d agents took %d%n ms", opinions.length, time);
    }

    public void simulateUndecidedStateDynamics() {
        long startTime = System.currentTimeMillis();
        int count = 0, index = 0;
        while (noConsensus(opinions)) {
            List<Integer> integers = randomNums(2);
            if (opinions[integers.get(0)] != opinions[integers.get(1)] && opinions[integers.get(1)] != 'U' && opinions[integers.get(0)] != 'U') {
                opinions[integers.get(0)] = 'U';
            } else if (opinions[integers.get(0)] == 'U' && opinions[integers.get(1)] != 'U') {
                opinions[integers.get(0)] = opinions[integers.get(1)];
            }
            ++count;
            if (count % (opinions.length/2) == 0) {
                printCSV(index, startTime, System.currentTimeMillis());
                ++index;
            }
        }
        printCSV(index, startTime, System.currentTimeMillis());
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
