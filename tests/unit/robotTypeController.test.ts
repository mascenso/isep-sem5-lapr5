import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';

import { Result } from '../../src/core/logic/Result';
import IRobotTypeDTO from '../../src/dto/IRobotTypeDTO';
import IRobotTypeService from '../../src/services/IServices/IRobotTypeService';
import RobotTypeController from "../../src/controllers/robotTypeController";
import Container from 'typedi';



describe('robot Type controller', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        this.timeout(6000);
        Container.reset();

        let robotTypeSchemaClass = require("../../src/persistence/schemas/robotTypeSchema").default;
        Container.set("robotTypeSchema", robotTypeSchemaClass);

        let robotTypeRepoClass = require("../../src/repos/robotTypeRepo").default;
        let robotTypeRepoInstance = Container.get(robotTypeRepoClass);
        Container.set("RobotTypeRepo", robotTypeRepoInstance);

        let robotTypeInstance = require("../../src/domain/robotType").RobotType;
        Container.set("RobotType", robotTypeInstance);

        let robotTypeServiceClass = require("../../src/services/robotTypeService").default;
        let robotTypeServiceInstance = Container.get(robotTypeServiceClass);
        Container.set("robotTypeService", robotTypeServiceInstance);

    });

    afterEach(function () {
        sinon.restore();
        sandbox.restore();
    });

    it('robotTypeController unit test using robotTypeService stub', async function () {
        // Arrange
        let body = {"id": '123', "designacao": "TypeX", "tipoTarefas": ["Outras"]};
  
        let req: Partial<Request> = {};
        req.body = body;
        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let robotTypeServiceInstance = Container.get("robotTypeService");
        sinon.stub(robotTypeServiceInstance, "createRobotType").returns(Result.ok<IRobotTypeDTO>({
            "id": req.body.id,
            "designacao": req.body.designacao,
            "tipoTarefas": req.body.tipoTarefas
        }));

        const ctrl = new RobotTypeController(robotTypeServiceInstance as IRobotTypeService);

        // Act
        await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            "id": req.body.id,
            "designacao": req.body.designacao,
            "tipoTarefas": req.body.tipoTarefas
        }));
    });

    it('robotTypeController unit test using robotTypeService mock', async function () {
        // Arrange
        let body = {"id": '123', "designacao": "TypeX", "tipoTarefas": ["Outras"]};
        let req: Partial<Request> = {};
        req.body = body;

        let res: Partial<Response> = {
            json: sinon.spy()
        };
        let next: Partial<NextFunction> = () => { };

        let robotTypeServiceInstance = Container.get("RobotTypeService");
        const robotTypeServiceMock = sinon.mock(robotTypeServiceInstance, "createRobotType")
        robotTypeServiceMock.expects("createRobotType")
            .once()
            .withArgs(sinon.match({ name: req.body.name }))
            .returns(Result.ok<IRobotTypeDTO>({
                "id": req.body.id,
                "designacao": req.body.designacao,
                "tipoTarefas": req.body.tipoTarefas
            }));

        const ctrl = new RobotTypeController(robotTypeServiceInstance as IRobotTypeService);

        // Act
        await ctrl.createRobotType(<Request>req, <Response>res, <NextFunction>next);

        // Assert
        robotTypeServiceMock.verify();
        sinon.assert.calledOnce(res.json);
        sinon.assert.calledWith(res.json, sinon.match({
            "id": req.body.id,
            "designacao": req.body.designacao,
            "tipoTarefas": req.body.tipoTarefas
        }));
    });

});


