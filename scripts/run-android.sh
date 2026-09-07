#!/bin/sh

set -eu

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
if ! "$adb_bin" get-state >/dev/null 2>&1; then
	echo "No authorized Android device found. Connect it, unlock it, and enable USB debugging." >&2
	exit 1
fi

cd "$repository_dir/android"
ANDROID_HOME="$android_sdk" \
ANDROID_SDK_ROOT="$android_sdk" \
JAVA_HOME="$java_home" \
PATH="$java_home/bin:$PATH" \
./gradlew --no-daemon :app:installDebug

"$adb_bin" shell am force-stop com.olivierh59500.multiscreen
"$adb_bin" shell am start -n com.olivierh59500.multiscreen/.MainActivity
