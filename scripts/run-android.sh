#!/bin/sh

set -eu

usage() {
	echo "Usage: $0 [--dck] [--build-only]" >&2
}

mobile_package=./mobile
build_only=false
for option in "$@"; do
	case "$option" in
		--dck) mobile_package=./dck/mobile ;;
		--build-only) build_only=true ;;
		--help|-h) usage; exit 0 ;;
		*) usage; exit 2 ;;
	esac
done
export EBITENMOBILE_PACKAGE="$mobile_package"

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repository_dir=$(CDPATH= cd -- "$script_dir/.." && pwd)
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
		if [ -x "$candidate/platform-tools/adb" ]; then
			android_sdk=$candidate
			break
		fi
	done
fi
if [ -z "$android_sdk" ] || [ ! -x "$android_sdk/platform-tools/adb" ]; then
	echo "Android SDK/adb not found. Set ANDROID_HOME or ANDROID_SDK_ROOT." >&2
	exit 1
fi

java_home=${JAVA_HOME:-}
if [ -z "$java_home" ]; then
	for candidate in \
		"/Applications/Android Studio.app/Contents/jbr/Contents/Home" \
		"/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home" \
		"/usr/local/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
	do
		if [ -x "$candidate/bin/java" ]; then
			java_home=$candidate
			break
		fi
	done
fi
if [ -z "$java_home" ] || [ ! -x "$java_home/bin/java" ]; then
	echo "JDK 17 not found. Set JAVA_HOME." >&2
	exit 1
fi

adb_bin="$android_sdk/platform-tools/adb"
if ! "$build_only" && ! "$adb_bin" get-state >/dev/null 2>&1; then
	echo "No authorized Android device found. Connect it, unlock it, and enable USB debugging." >&2
	exit 1
fi

if "$build_only"; then
	EBITENMOBILE_TARGET=${EBITENMOBILE_TARGET:-android/arm64}
else
	device_abi=$("$adb_bin" shell getprop ro.product.cpu.abi | tr -d '\r')
	case "$device_abi" in
		arm64-v8a) EBITENMOBILE_TARGET=android/arm64 ;;
		armeabi-v7a) EBITENMOBILE_TARGET=android/arm ;;
		x86) EBITENMOBILE_TARGET=android/386 ;;
		x86_64) EBITENMOBILE_TARGET=android/amd64 ;;
		*) EBITENMOBILE_TARGET=android ;;
	esac
fi
export EBITENMOBILE_TARGET

cd "$repository_dir/android"
gradle_task=:app:installDebug
if "$build_only"; then
	gradle_task=:app:assembleDebug
fi
ANDROID_HOME="$android_sdk" \
ANDROID_SDK_ROOT="$android_sdk" \
JAVA_HOME="$java_home" \
PATH="$java_home/bin:$PATH" \
EBITENMOBILE_TARGET="$EBITENMOBILE_TARGET" \
./gradlew --no-daemon :app:clean "$gradle_task"

if "$build_only"; then
	exit 0
fi

"$adb_bin" shell am force-stop com.olivierh59500.multiscreen
"$adb_bin" shell am start -n com.olivierh59500.multiscreen/.MainActivity
