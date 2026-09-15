#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_dir=$(CDPATH= cd -- "$script_dir/.." && pwd)
output_file=${1:-"$repository_dir/android/app/libs/multiscreen.aar"}

android_sdk=${ANDROID_HOME:-${ANDROID_SDK_ROOT:-}}
if [ -z "$android_sdk" ]; then
	user_home_dir=${HOME:-}
	for candidate in \
		"$repository_dir/.android-sdk" \
		"$user_home_dir/Library/Android/sdk" \
		"$user_home_dir/Android/Sdk" \
		"/opt/homebrew/share/android-commandlinetools" \
		"/usr/local/share/android-sdk"
	do
		if [ -d "$candidate/platforms" ]; then
			android_sdk=$candidate
			break
		fi
	done
fi
if [ -z "$android_sdk" ] || [ ! -d "$android_sdk/platforms" ]; then
	echo "Android SDK not found. Set ANDROID_HOME or ANDROID_SDK_ROOT." >&2
	exit 1
fi

java_home=${JAVA_HOME:-}
if [ -z "$java_home" ]; then
	for candidate in \
		"/Applications/Android Studio.app/Contents/jbr/Contents/Home" \
		"/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home" \
		"/usr/local/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
	do
		if [ -x "$candidate/bin/javac" ]; then
			java_home=$candidate
			break
		fi
	done
fi
if [ -n "$java_home" ]; then
	JAVA_HOME=$java_home
	PATH="$java_home/bin:$PATH"
	export JAVA_HOME PATH
elif ! javac -version >/dev/null 2>&1; then
	echo "JDK 17 not found. Set JAVA_HOME." >&2
	exit 1
fi

ebitenmobile_bin=${EBITENMOBILE:-}
if [ -n "$ebitenmobile_bin" ] && [ ! -x "$ebitenmobile_bin" ]; then
	echo "EBITENMOBILE does not point to an executable: $ebitenmobile_bin" >&2
	exit 1
fi

run_ebitenmobile() {
	if [ -n "$ebitenmobile_bin" ]; then
		"$ebitenmobile_bin" "$@"
	else
		# The tool directive in go.mod keeps the generator aligned with the
		# Ebitengine library version used by the application.
		go tool ebitenmobile "$@"
	fi
}

ebitenmobile_target=${EBITENMOBILE_TARGET:-android}

android_ndk=${ANDROID_NDK_HOME:-}
if [ -z "$android_ndk" ] && [ -d "$android_sdk/ndk" ]; then
	android_ndk=$(find "$android_sdk/ndk" -mindepth 1 -maxdepth 1 -type d | sort | tail -n 1)
fi

go_cache=${GOCACHE:-${TMPDIR:-/tmp}/multiscreen-go-build-cache}
mkdir -p "$(dirname -- "$output_file")" "$go_cache"

cd "$repository_dir"
ANDROID_HOME="$android_sdk" \
ANDROID_NDK_HOME="$android_ndk" \
GOCACHE="$go_cache" \
run_ebitenmobile bind \
	-target "$ebitenmobile_target" \
	-androidapi 23 \
	-javapkg com.olivierh59500.multiscreen \
	-trimpath \
	-ldflags="-s -w" \
	-o "$output_file" \
	./mobile
