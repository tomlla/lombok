/*
 * Copyright (C) 2013-2014 The Project Lombok Authors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package lombok.core;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import lombok.core.configuration.BubblingConfigurationResolver;
import lombok.core.configuration.ConfigurationKey;
import lombok.core.configuration.ConfigurationProblemReporter;
import lombok.core.configuration.ConfigurationResolver;
import lombok.core.configuration.ConfigurationResolverFactory;
import lombok.core.configuration.FileSystemSourceCache;
import lombok.core.debug.FileLog;

public class LombokConfiguration {
	
	public static void writeDebugText(final String message) {
		writeDebugText(Arrays.asList(message));
	}
	
	public static void writeDebugText(final List<String> messageList) {
		FileWriter fw = null;
		try {
			fw = new FileWriter(new File("/tmp/lombok-debug.txt"), true);
			for (final String msg : messageList) {
				FileLog.log(msg);
				System.out.println(msg);
				fw.write(msg + "\n");
				final Logger logger = java.util.logging.Logger.getLogger("lombok_debug_tomlla");
				logger.info(msg);
			}
		} catch (final IOException e) {
			throw new RuntimeException(e);
		} finally {
			if (fw != null) {
				try {
					fw.close();
				} catch (final IOException e) {
					throw new RuntimeException(e);
				}
			}
		}
		
	}
	
	private static final ConfigurationResolver NULL_RESOLVER = new ConfigurationResolver() {
		@SuppressWarnings("unchecked") @Override public <T> T resolve(final ConfigurationKey<T> key) {
			if (key.getType().isList()) return (T) Collections.emptyList();
			return null;
		}
	};
	
	private static FileSystemSourceCache cache = new FileSystemSourceCache();
	private static ConfigurationResolverFactory configurationResolverFactory;
	
	static {
		if (System.getProperty("lombok.disableConfig") != null) {
			
			final List<String> messages = Arrays.asList("--", "lombok.disableConfig = True", "---");
			writeDebugText(messages);
			configurationResolverFactory = new ConfigurationResolverFactory() {
				@Override public ConfigurationResolver createResolver(final AST<?, ?, ?> ast) {
					return NULL_RESOLVER;
				}
			};
		} else {
			final List<String> messages = Arrays.asList("--", "lombok.disableConfig = Not Set(config system is active.)", "---");
			writeDebugText(messages);
			configurationResolverFactory = createFileSystemBubblingResolverFactory();
		}
	}
	
	private LombokConfiguration() {
		// prevent instantiation
	}
	
	public static void overrideConfigurationResolverFactory(final ConfigurationResolverFactory crf) {
		if (crf != null) {
			writeDebugText("overrideConfigurationResolverFactory");
		}
		configurationResolverFactory = crf == null ? createFileSystemBubblingResolverFactory() : crf;
	}
	
	static <T> T read(final ConfigurationKey<T> key, final AST<?, ?, ?> ast) {
		return configurationResolverFactory.createResolver(ast).resolve(key);
	}
	
	private static ConfigurationResolverFactory createFileSystemBubblingResolverFactory() {
		return new ConfigurationResolverFactory() {
			@Override public ConfigurationResolver createResolver(final AST<?, ?, ?> ast) {
				return new BubblingConfigurationResolver(cache.sourcesForJavaFile(ast.getAbsoluteFileLocation(), ConfigurationProblemReporter.CONSOLE));
			}
		};
	}
}
