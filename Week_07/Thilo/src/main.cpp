#include <curl/curl.h>
#include <fstream>
#include <iostream>
#include <mutex>
#include <set>
#include <string>
#include <thread>
#include "json.hpp"

using json = nlohmann::json;

std::once_flag curl_init_flag;

long requests = 0;

size_t callback(char* buf, size_t size, size_t nmemb, void* user_data) {
	if (user_data) {
		auto data = static_cast<std::string*>(user_data);
		for (long i = 0; i < size * nmemb; i++) {
			data->push_back(buf[i]);
		}

		return size * nmemb;
	}

	return size_t(0);
}

std::string read_from_url(std::string url) {
	CURL* curl;

	std::call_once(curl_init_flag,
	               std::bind(curl_global_init, CURL_GLOBAL_ALL));

	curl = curl_easy_init();

	std::string content;

	curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, &content);

	curl_easy_perform(curl);

	curl_easy_cleanup(curl);

	requests++;
	std::cout << "Request count: " << requests << std::endl;

	return content;
}

json read_collection(const std::string url, const std::string postfix) {
	json j;

	bool status_ok = false;
	do {
		status_ok = true;
		auto str = read_from_url(url + postfix);

		std::cout << "Read collection" << url << postfix << std::endl;
		std::cout << str << std::endl;

		try {
			j = j.parse(str);

			if (j.count("ErrorCode") == 1) {
				status_ok = false;
			}
		} catch (...) {
			status_ok = false;
			std::cerr << "Issue while parsing " << url << postfix
			          << ", retrying..." << std::endl;
		}
	} while (!status_ok);

	return j;
}

json read_detail(const std::string url, const long id, const std::string prefix,
                 const std::string postfix = "") {
	bool status_ok = false;
	json res;
	do {
		status_ok = true;
		std::ostringstream os;
		os << url << prefix << id << postfix;

		auto detail_a = read_from_url(os.str());

		std::cout << "Read detail " << os.str() << std::endl;
		std::cout << detail_a << std::endl;

		try {
			res = json::parse(detail_a);

			// If an error is returned and suggests to try again, try again
			if (res.count("ErrorCode") == 1 && res.count("Description") == 1) {
				if (res.at("Description")
				        .get<std::string>()
				        .find("Es ist ein interner Datenbankfehler "
				              "aufgetreten") != std::string::npos) {
					status_ok = false;
				}
			}
		} catch (...) {
			status_ok = false;
			std::cerr << "Issue while parsing " << os.str() << ", retrying..."
			          << std::endl;
			std::this_thread::sleep_for(std::chrono::milliseconds(100));
		}
	} while (!status_ok);
	return res;
}

// Helper: Save string to file
void save_to_file(std::string filename, std::string content) {
	std::ofstream file(filename);
	if (file.is_open()) {
		file << content;
		file.close();
	}
}

// Helper: Dump map<long, json> to single json object
json pack_map(const std::map<long, json>& input) {
	json j;

	for (const auto& e : input) {
		j.push_back(e.second);
	}

	return j;
}

int main() {
	std::string url = "http://cqbsapiquiz.azurewebsites.net/api/values/";

	std::cout << "READ ENTITY FOO" << std::endl;

	auto j = read_collection(url, "Foo/");

	json all_foo;
	for (auto& e : j) {
		auto detail = read_detail(url, e.at("Id"), "Foo/");

		if (detail.count("id") + detail.count("name") +
		        detail.count("isClosed") + detail.count("details") +
		        detail.count("childIds") ==
		    5) {
			if (detail.at("details").count("name") +
			        detail.at("details").count("description") +
			        detail.at("details").count("hint") +
			        detail.at("details").count("id") ==
			    4) {
				all_foo.push_back(detail);
			} else {
				std::cerr << "Wrong data format: Not all details" << std::endl;
				std::cerr << detail << std::endl;
			}
		} else {
			std::cerr << "Wrong data format" << std::endl;
			std::cerr << detail << std::endl;
		}
	}

	save_to_file("foo.json", all_foo.dump());

	std::cout << "READ ENTITY A" << std::endl;

	j = read_collection(url, "AEntity/");

	std::map<long, json> all_a;
	for (auto& e : j) {
		auto detail = read_detail(url, e.at("Id"), "AEntity/");

		if (detail.count("id") + detail.count("name") + detail.count("max") +
		        detail.count("min") + detail.count("cEntityId") ==
		    5) {
			all_a.insert(std::pair<long, json>(detail["id"], detail));
		} else {
			std::cerr << "Wrong data format" << std::endl;
			std::cerr << detail << std::endl;
		}
	}

	save_to_file("aentity.json", pack_map(all_a).dump());

	std::cout << "READ ENTITY B" << std::endl;

	j = read_collection(url, "BEntity/");

	std::map<long, json> all_b;
	for (auto& e : j) {
		auto detail = read_detail(url, e.at("Id"), "BEntity/");
		if (detail.count("id") + detail.count("isAwesome") +
		        detail.count("isTehSuck") + detail.count("cEntityId") +
		        detail.count("name") ==
		    5) {
			all_b.insert(std::pair<long, json>(detail["id"], detail));
		} else {
			std::cerr << "Wrong data format" << std::endl;
			std::cerr << detail << std::endl;
		}
	}

	save_to_file("bentity.json", pack_map(all_b).dump());

	// Build a list of all CEntity that ocurr in A and B
	std::set<long> all_c_ids;

	for (auto a : all_a) {
		if (a.second["cEntityId"].is_number_integer()) {
			all_c_ids.insert(a.second["cEntityId"].get<long>());
		}
	}

	for (auto a : all_b) {
		if (a.second["cEntityId"].is_number_integer()) {
			all_c_ids.insert(a.second["cEntityId"].get<long>());
		}
	}

	std::cout << "READ ENTITY C" << std::endl;

	std::map<long, json> all_c;
	for (auto& e : all_c_ids) {
		auto detail = read_detail(url, e, "", "/CEntity");
		if (detail.count("id") + detail.count("description") +
		        detail.count("hint") ==
		    3) {
			all_c.insert(std::pair<long, json>(detail["id"], detail));
		} else {
			std::cerr << "Wrong data format:" << std::endl;
			std::cerr << detail << std::endl;
		}
	}

	save_to_file("centity.json", pack_map(all_c).dump());

	// Merge cEntities into aEntities
	for (auto& e : all_a) {
		if (e.second["cEntityId"].is_number_integer()) {
			auto a = all_c.find(e.second["cEntityId"].get<long>());
			if (a != all_c.cend()) {
				e.second["cEntityId"] = (*a).second;
			}
		}
	}
	save_to_file("aentity_merged.json", pack_map(all_a).dump());

	// Merge cEntities into bEntities
	for (auto& e : all_b) {
		if (e.second["cEntityId"].is_number_integer()) {
			auto a = all_c.find(e.second["cEntityId"].get<long>());
			if (a != all_c.cend()) {
				e.second["cEntityId"] = (*a).second;
			}
		}
	}

	save_to_file("bentity_merged.json", pack_map(all_b).dump());

	std::cout << "MERGING EVERYTHING TOGETHER" << std::endl;

	// Merge everything into one json object
	for (auto& f : all_foo) {
		for (auto& e : f["childIds"]) {
			if (e.is_number_integer()) {
				auto a = all_a.find(e.get<long>());
				if (a != all_a.cend()) {
					e = (*a).second;
				}
			}
			if (e.is_number_integer()) {
				auto b = all_b.find(e.get<long>());
				if (b != all_b.cend()) {
					e = (*b).second;
				}
			}
		}
	}

	save_to_file("foo_merged.json", all_foo.dump());

	std::cout << "Foo merged length: " << all_foo.dump().length() << std::endl;

	return 0;
}
