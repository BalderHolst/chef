{ pkgs, lib }:
let

    script-msg = ''
        # This script was generated by Nix.
        # To make changes edit the `jobs.nix` file.

    '';


    jobScriptString = job: level: ''
        echo "${ lib.strings.replicate level "--" }->> Running '${job.name}'"
        # Dependencies for ${job.name}
        ${builtins.concatStringsSep "\n" (map (j: jobScriptString j (level+1)) job.depends)}
        # Run ${job.name} job
        ${job.script}
    '';

    tab-indent = s: (if s == "" then "" else "\t") + (builtins.concatStringsSep "\n\t" (
        builtins.filter (e: builtins.typeOf e != "list") (builtins.split "\n" s)
    ));

in
rec {

    mkScriptBin  = job: pkgs.writeShellScriptBin job.name (script-msg + (jobScriptString job 0));
    mkScript     = job: pkgs.writeShellScript    job.name (script-msg + (jobScriptString job 0));

    mkMakefile = jobs: pkgs.writeText "Makefile" (''
        # This Makefile was generated by Nix.
        # To make changes edit the `jobs.nix` file.

        main: help

        all: ${ builtins.concatStringsSep " " (map (j: "${j.name}") jobs) }

        help:
        ${ tab-indent /*bash*/ ''
            @echo "usage: make <task>"
            @echo ""
            @echo "Available Tasks:"
            ${ (builtins.concatStringsSep "\n" (map (j:  "@echo -e '\t${j.name}'") jobs)) }
            @echo -e "\nUse 'make help' command to show this list."
        ''}

    '' +
        (builtins.concatStringsSep "\n\n" (map (job: ''
            ${job.name}: ${ builtins.concatStringsSep " " (map (j: "${j.name}") job.depends) }
            ${
                if job.script == "" then "" else "\t"
            }${
                builtins.concatStringsSep "\n\t" (
                    builtins.filter (e: builtins.typeOf e != "list") (builtins.split "\n" job.script)
                )}
        '') jobs))
    );

    mkHelpScriptBin = jobs: pkgs.writeShellScriptBin "help" ''
        echo "Available Tasks:"
        ${ builtins.concatStringsSep "\n" (map (j:  "echo -e '\t${j.name}'") jobs) }
        echo -e "\nUse 'help' command to show this list."
    '';

    mkJob = name: { script ? "", depends ? [], }: {
        name = name;
        script = script;
        depends = depends;
    };

    jobSeq = name: seq: mkJob name { depends = seq; };

}
