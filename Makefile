fmt:
	nixfmt **/*.nix

flake-update:
	nix flake update
	notify-send "NixOS: flake update finished!"

switch-os: fmt
	# git diff --exit-code
	rm /home/curtain/.config/fontconfig/conf.d/10-hm-fonts.conf || true
	sudo -S nixos-rebuild switch --max-jobs 16 --cores 16 --flake /home/curtain/workspace/nixos-configuration/.#fuzzdog --verbose --show-trace --print-build-logs
	notify-send "NixOS: OS switch finished!"

switch-hm: fmt
	# git diff --exit-code
	home-manager switch --flake .#fuzzdog --verbose --show-trace --print-build-logs
	notify-send "NixOS: home-manager swtich finished!"

boot: fmt
	git diff --exit-code
	sudo -S nixos-rebuild boot --flake /home/curtain/workspace/nixos-configuration/.#fuzzdog --verbose --show-trace
	notify-send "NixOS: boot finished!"
