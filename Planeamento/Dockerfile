# Use a SWIPL base image
FROM swipl:latest

# Set the working directory
WORKDIR /app

# Copy only the necessary Prolog files to the container
COPY server.pl /app/
COPY AlgoritmoGenetico.pl /app/
COPY Algoritmos.pl /app/
COPY analiseComplexidade.pl /app/
COPY BC_RobDroneGo.pl /app/
COPY BC_trajectos.pl /app/
COPY main.pl /app/
COPY Percurso_Robots.pl /app/
COPY . /app

# Expose the port on which your SWIPL server will run
EXPOSE 8082

# Defina uma variável de ambiente para evitar problemas com o fork no SWI-Prolog
ENV SWIPL_SAFE_FORK true

ENTRYPOINT ["swipl"]
CMD ["/app/server.pl",  "--user=daemon", "--fork=false", "--port=8082"]
