# broadcasterl

Both proof of concept and simple library for erlang service discovery using a
Nginx balancing UDP across several nodes (instead of UDP broadcast frames)

Any UDP reverse proxy will do.

## Replay attacks

This module is vulnerable to replay attacks, meaning that anyone sending an UDP
package that was previously captured will trigger a `binary_to_atom` call.

This shouldn't be an issue because the atom has been correct at some point in
time (packages are protected by sending them encrypted with a key derived of
erlang's cookie), but if you create a huge amount of ephemeral nodes in your
cluster with the same cookie, anyone with access to the traffic will be able to
trigger the same connections.

Protecting against this is not difficult, but it requires changing from UDP to
TCP, increasing the traffic. I don't think this risk is relevant enough.

## How to use as a proof of concept

Requires having both `docker-compose` and `make`, `make` is only used for
conveniently compose `docker-compose` commands.
```
make release
docker-compose up
```

## How to use as a library

You can use rebar3's `git_subdir` dependency, just point it to
`apps/service_discovery`.

```
{deps, [
    {service_discovery, {git_subdir,"https://github.com/kuroneer/broadcasterl.git", {tag,"1.0.0"}, "apps/service_discovery"}}
]}.
```

The application has the following configurable values:

```
discovery_port (6350): Which port to listen and send UDP packets
discovery_address (["erlang-discovery"]): Which hosts to send UDP packets to (a
list is allowed to support migrations)
listen_options ([{ip, {0,0,0,0}}]): Listen options for the local UDP socket
broadcast_min (1): Broadcast batches minimum size
broadcast_max (3): Broadcast batches maximum size
timer_min (1000): Min time between broadcasts
timer_max (3000): Max time between broadcasts
```

Keep in mind that as the cluster grows, the timers are increased to try to
maintain the broadcast rate cluster-wise. (A cluster with two nodes will take
twice as much time as a single-node cluster to broadcast a message). This is
intended, as any connection to a single node of the cluster will connect to
the whole cluster.

## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)

