---
layout: post
title: K8s, AWS loadbalancers and the limits of human sanity
categories: [Programming]
---

I was working with K8s and AWS when I wanted to change the settings of a loadbalancer. I use a yaml and some auto-deploy features, so I went ahead and updated the deployment to use only an internal network loadbalancer on UDP. I apply and...

```bash
namespace/my-app unchanged
serviceaccount/my-ksa unchanged
deployment.apps/my-deploy unchanged
service/my-svc unchanged
service/my-rtsp unchanged
```

No change. Strange. let's check AWS.



A syntax, error, lets compare my yaml with the docks.

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-deploy
  namespace: my-app
  annotations:
    service.beta.kubernetes.io/aws-load-balancer-type: nlb
    service.beta.kubernetes.io/aws-load-balancer-internal: "true"
spec:
  ports:
    - port: 554
      targetPort: 554
      name: my-stream
      protocol: UDP
  type: LoadBalancer
  selector:
    app: my-app
```

```yaml
apiVersion: v1
kind: Service
metadata:
  name: my-service
spec:
  selector:
    app.kubernetes.io/name: MyApp
  ports:
    - protocol: TCP
      port: 80
      targetPort: 9376
  clusterIP: 10.0.171.239
  type: LoadBalancer
status:
  loadBalancer:
    ingress:
    - ip: 192.0.2.127
```

It also adds that you can enable nlb with:

```yaml
    metadata:
      name: my-service
      annotations:
        service.beta.kubernetes.io/aws-load-balancer-type: "nlb"
```

Perhaps I need to specify the ingress IP and clusterIP? That shouldn't matter with our loadbalancer dynamically routing to the right container? Thankfully there's a wealth of information about this available on the internet, and this certainly isn't an obscure bug referenced only by a [single blog post](https://ben-lab.github.io/kubernetes-UDP-TCP-bug-same-port/) that has gone unfixed FOR 7 YEARS. Situations like those are fairytales designed to scare lesser programmers...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<div align="center">

<img src="https://i.redd.it/c7slncep71q41.jpg" class="img-responsive" alt="Byte rotations">

</div>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## 7 Years and counting...

This issue was [opened in 2016](https://github.com/kubernetes/kubernetes/issues/39188) and still hasn't been fixed. Kubernetes checks the keys to quickly determine what does and doesn't need updating. Instead of comparing something like a hash of all the elements, the key Kubernetes checks is the "port" key. In this scenario, the port doesn't change. this means if you add some new service later or attempt to change any of the details relating to the deployment, you'll find yourself stuck forever. Simply deleting the deployment before every apply fixes this, but we now need to rebuild all of our AWS infrastructure. May Stallman bless your future bug fixing and save you from the days of searching that I've done.