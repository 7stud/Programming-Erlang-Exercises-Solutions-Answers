![graph of runtimes v. processes](https://github.com/7stud/Programming-Erlang-Exercises-Solutions-Answers/blob/master/Chapter%2012/runtimes.png)

### *What can you deduce from the graph?*
You can create a lot of processes with erlang!  It looks like the walltime goes up faster than the runtime, so the CPU must be waiting around more when the erlang VM creates more processes.
