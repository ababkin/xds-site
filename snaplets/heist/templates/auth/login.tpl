<bind tag="action">/login</bind>
<bind tag="method">post</bind>
<bind tag="submitText">Login</bind>

<apply template="/layout/base">
  <div class="col-md-6">
    <h2>Log in</h2>

      <flash/>

      <apply template="form"/>
    <a href="/signup">Sign up</a>
    <br>
    <a href="/users">Forgot your password?</a>
    <br>
  </div>
</apply>
