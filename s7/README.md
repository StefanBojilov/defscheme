
```html

<div> This is s7 scheme interpreter from: https://ccrma.stanford.edu/software/snd/ <br>
Quite good for what it is.
</div>
<br>
<div>
 To compile with repl:<br>
<pre>
 touch mus-config.h     ### if compiled without the snd package, as it's here
 gcc s7.c -o repl -DWITH_MAIN -I. -O2 -g -ldl -lm -Wl,-export-dynamic ### may take some time on slower machines
</pre>
</div>

```

