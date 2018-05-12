package com.day7.app;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.File;

public class App
{
    public static void main( String[] args )
    {
        File f = new File("src/main/java/com/day7/app/input");
        parseFile(f);
    }

    public static void parseFile(File file) {
        Map<String, Integer> weights = new HashMap<>();
        Map<String, Set<String>> children = new HashMap<>();
        Set<String> union = new HashSet<>();
        try(BufferedReader br = new BufferedReader(new FileReader(file))) {
            String line;
            // Parsing
            while((line = br.readLine()) != null) {
                if(line.trim().length() > 0) {
                    String[] parChildren = line.split("\\s+->\\s+");
                    String[] parValue = parChildren[0].split(" ");
                    String parent = parValue[0];

                    // Split off the weight and add it to a separate map
                    String sub = parValue[1];
                    sub = sub.substring(sub.indexOf("(") + 1);
                    int num = Integer.parseInt(sub.substring(0, sub.indexOf(")")));
                    weights.put(parent, num);

                    children.put(parent, new HashSet<>());
                    if(parChildren.length > 1) {
                        union.addAll(Arrays.asList(parChildren[1].split(", ")));
                        children.get(parent).addAll(Arrays.asList(parChildren[1].split(", ")));
                    }
                }
            }

            // Difference of all items with all children gives us one element: the root
            Set<String> keys = children.keySet().stream().map(String::new).collect(Collectors.toSet());
            keys.removeAll(union);
            Iterator<String> it = keys.iterator();
            if(it.hasNext()) {
                String root = it.next();
                System.out.println("The root of the tree is: " + root);

                Map<String, Integer> treeSums = subTreeSums(weights, children, root, new HashMap<>());
                int diff = 0;
                // Calculate diff
                Map<Integer, Set<String>> freq = new HashMap<>();
                for(String ch : children.get(root)) {
                    freq.computeIfAbsent(treeSums.get(ch), k -> new HashSet<String>()).add(ch);
                }
                Entry<Integer, Set<String>> wrongEntry = Collections.min(freq.entrySet(), Comparator.comparing(e -> e.getValue().size()));
                freq.remove(wrongEntry.getKey());
                diff = freq.keySet().iterator().next() - wrongEntry.getKey();

                String prev = root;
                String el = wrongEntry.getValue().iterator().next();
                while(!children.get(el).isEmpty() && prev != el) {
                    prev = el;
                    el = getWrongSubtree(children, treeSums, el, diff);
                }
                System.out.println("Element `" + el + "` should have the weight: " + (weights.get(el) + diff));
            }
        } catch (IOException e) {
                e.printStackTrace();
            }
    }

    public static String getWrongSubtree(Map<String, Set<String>> elements, Map<String, Integer> weights, String el, int diff) {
        Set<String> children = elements.get(el);
        Set<Integer> values = children.stream().map(e -> weights.get(e)).collect(Collectors.toSet());
        if(values.size() == 1) {
            return el;
        }
        if(diff < 0) {
            return Collections.max(children, Comparator.comparing(e -> weights.get(e)));
        } else {
            return Collections.min(children, Comparator.comparing(e -> weights.get(e)));
        }
    }

    public static Map<String, Integer> subTreeSums(Map<String, Integer> weights, Map<String, Set<String>> elements, String el, Map<String, Integer> sums) {
        int sum = weights.get(el);
        Set<String> children = elements.get(el);
        if(children.isEmpty()) {
            sums.put(el, sum);
        } else {
            for(String child : children) {
                sum += subTreeSums(weights, elements, child, sums).get(child);
            }
            sums.put(el, sum);
        }
        return sums;
    }
}
