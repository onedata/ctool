%%%--------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for certificates creation,
%%% manipulation and signing.
%%% @end
%%%--------------------------------------------------------------------
-module(cert_utils).
-author("Lukasz Opiola").

%% API
-export([load_der/1, load_ders_in_dir/1]).
-export([pem_to_der/1]).
-export([create_key/1]).
-export([create_csr/3]).
-export([create_signed_webcert/5]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Reads the certificate file under given path and returns the contents in
%% DER format.
%% @end
%%--------------------------------------------------------------------
-spec load_der(file:filename_all()) -> public_key:der_encoded().
load_der(Path) ->
    {ok, CertPem} = file:read_file(Path),
    pem_to_der(CertPem).


%%--------------------------------------------------------------------
%% @doc
%% Reads all certificate files in given directory and returns their contents in
%% DER format.
%% @end
%%--------------------------------------------------------------------
-spec load_ders_in_dir(file:filename_all()) -> [public_key:der_encoded()].
load_ders_in_dir(DirPath) ->
    {ok, CertPems} = file_utils:read_files({dir, DirPath}),
    lists:map(fun pem_to_der/1, CertPems).


%%--------------------------------------------------------------------
%% @doc
%% Converts certificate from pem to der format.
%% @end
%%--------------------------------------------------------------------
-spec pem_to_der(file:filename_all()) -> public_key:der_encoded().
pem_to_der(CertPem) ->
    PemEntries = public_key:pem_decode(CertPem),
    {'Certificate', CertDer, _} = lists:keyfind('Certificate', 1, PemEntries),
    CertDer.


%%--------------------------------------------------------------------
%% @doc
%% Creates an RSA key under given path.
%% @end
%%--------------------------------------------------------------------
-spec create_key(file:filename_all()) -> ok.
create_key(Path) ->
    shell_cmd([
        "openssl genrsa",
        "-out", Path,
        "2048"
    ]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Creates a Certificate Signing Request under given path, with
%% specified Common Name (CN).
%% Used only for test/development purposes.
%% @end
%%--------------------------------------------------------------------
-spec create_csr(KeyPath :: file:filename_all(),
    OutputPath :: file:filename_all(), string() | binary()) -> ok.
create_csr(KeyPath, OutputPath, CommonName) ->
    [] = shell_cmd([
        "openssl req",
        "-new",
        "-key", KeyPath,
        "-out", OutputPath,
        "-subj", str_utils:format("'/C=PL/L=OneDataTest/O=OneDataTest/CN=~s'", [
            CommonName
        ])
    ]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Creates a web server key/cert pair and signs the cert with given CA.
%% Used only for test/development purposes.
%% @end
%%--------------------------------------------------------------------
-spec create_signed_webcert(KeyPath :: file:filename_all(),
    CertPath :: file:filename_all(), Hostname :: string() | binary(),
    CaKeyPath :: file:filename_all(), CaCertPath :: file:filename_all()) -> ok.
create_signed_webcert(KeyPath, CertPath, Hostname, CaKeyPath, CaCertPath) ->
    {Root, ConfigFile} = create_temp_ca_dir(),
    CsrPath = filename:join(Root, "temp.csr"),
    create_key(KeyPath),
    create_csr(KeyPath, CsrPath, Hostname),
    shell_cmd(["openssl ca -batch",
        "-config", ConfigFile,
        "-extensions server_cert ",
        "-days 3650 -notext",
        "-keyfile", CaKeyPath,
        "-cert", CaCertPath,
        "-in", CsrPath,
        "-out", CertPath
    ]),
    ok.

%%%===================================================================
%%% Private functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a temp directory and all files required for cert signing by a CA
%% cert.
%% @end
%%--------------------------------------------------------------------
-spec create_temp_ca_dir() ->
    {Root :: file:filename_all(), ConfigFile :: file:filename_all()}.
create_temp_ca_dir() ->
    Root = utils:mkdtemp(),
    ConfigFile = filename:join(Root, "openssl.cfg"),
    IndexFile = filename:join(Root, "index.txt"),
    SerialFile = filename:join(Root, "serial"),
    RandomSerial = httpd_util:integer_to_hexlist(999999999 + rand:uniform(999999999)),
    file:write_file(ConfigFile, openssl_cnf(Root)),
    file:write_file(IndexFile, <<"">>),
    file:write_file(SerialFile, RandomSerial),
    {Root, ConfigFile}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Joins a shell command split into tokens and executes it.
%% @end
%%--------------------------------------------------------------------
-spec shell_cmd([string()]) -> string().
shell_cmd(Tokens) ->
    os:cmd(string:join(Tokens, " ")).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contents of openssl.cfg file that will be used during signing by CA.
%% @end
%%--------------------------------------------------------------------
-spec openssl_cnf(file:filename_all()) -> string().
openssl_cnf(Home) -> "
HOME                   = " ++ Home ++ "
default_ca             = ca

[ ca ]
dir                    = $HOME
certs                  = $dir
crl_dir                = $dir
database               = $dir/index.txt
new_certs_dir          = $dir
serial                 = $dir/serial
crl                    = $dir/crl.pem
RANDFILE               = $dir/.rand
x509_extensions        = server_cert
default_days           = 3650
default_crl_days       = 30
default_md             = sha1
preserve               = no
policy                 = policy_anything

[ policy_anything ]
countryName            = optional
stateOrProvinceName    = optional
localityName           = optional
organizationName       = optional
organizationalUnitName = optional
commonName             = supplied
emailAddress           = optional

[ server_cert ]
basicConstraints       = CA:FALSE
nsCertType             = server
nsComment              = 'OpenSSL Generated Server Certificate'
subjectKeyIdentifier   = hash
authorityKeyIdentifier = keyid,issuer:always
extendedKeyUsage       = serverAuth
keyUsage               = digitalSignature, keyEncipherment".