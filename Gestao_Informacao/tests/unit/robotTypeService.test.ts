import 'reflect-metadata';

import * as sinon from 'sinon';
import { expect } from 'chai';
import { NextFunction, Request } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import RobotTypeService from "../../src/services/robotTypeService";
import { RobotType } from '../../src/domain/robotType-agg/robotType';
import TaskType from '../../src/enums/taskType';

describe('robot Type service', function () {
    const sandbox = sinon.createSandbox();

    beforeEach(function () {
        this.timeout(6000);
        Container.reset();
        let robotTypeSchemaClass = require("../../src/persistence/schemas/robotTypeSchema").default;
        Container.set("robotTypeSchema", robotTypeSchemaClass);

        let robotTypeRepoClass = require("../../src/repos/robotTypeRepo").default;
        let robotTypeRepoInstance = Container.get(robotTypeRepoClass);
        Container.set("RobotTypeRepo", robotTypeRepoInstance);

        let robotTypeInstance = require("../../src/domain/robotType-agg/robotType").RobotType;
        Container.set("RobotType", robotTypeInstance);

        let robotTypeServiceClass = require("../../src/services/robotTypeService").default;
        let robotTypeServiceInstance = Container.get(robotTypeServiceClass);
        Container.set("RobotTypeService", robotTypeServiceInstance);

    });

    afterEach(function () {
        sinon.restore();
        sandbox.restore();
    });

    it('robotTypeService unit test using robotType and robotTypeRepo stubs (createRobotType)', async function () {
        // Arrange
        let body = {"id": '123', "designacao": "TypeX", "tipoTarefas": ["Outras"]};
        let req: Partial<Request> = {};
        req.body = body;

        let robotTypeInstance = Container.get("RobotType");
        const robotTypeStub = sinon.stub(robotTypeInstance, "create").returns(Result.ok({
            "id": req.body.id,
            "designacao": req.body.designacao,
            "tipoTarefas": req.body.tipoTarefas
        }));

        let robotTypeRepoInstance = Container.get("RobotTypeRepo");
        const robotTypeRepoStub = sinon.stub(robotTypeRepoInstance, "save").returns(new Promise<RobotType>((resolve, reject) => {
            resolve(RobotType.create({
                "id": req.body.id,
                "designacao": req.body.designcao,
                "tipoTarefas": req.body.tipoTarefas
            }).getValue())
        }));

        const service = new RobotTypeService(Container.get("RobotTypeRepo"));

        const robotTypeDTO = { "id": "123", "designacao": 'TypeX', "tipoTarefas": [TaskType.Task3] };

        // Act
        await service.createRobotType(robotTypeDTO);

        // Assert
        sinon.assert.called(robotTypeStub);
        sinon.assert.calledOnce(robotTypeRepoStub);
        sinon.assert.calledWith(robotTypeStub, sinon.match(robotTypeDTO));

    });

    /*
    it('robotTypeService unit test using robot type and robotTypeRepo stubs (updateRobotType)', async function () {
        // Arrange
        let body = {"id": '123', "designacao": "TypeX", "tipoTarefas": ["Outras"]};
        let req: Partial<Request> = {};
        req.body = body;

        let robotTypeRepoInstance = Container.get("RobotTypeRepo");
        const robotTypeStub = sinon.stub(robotTypeRepoInstance, "findByDesignationOrTaskType").returns(new Promise<RobotType>((resolve, reject) => {
            resolve(RobotType.create({
                "id": req.body.id,
                "designacao": req.body.designacao,
                "tipoTarefas": req.body.tipoTarefas
            }).getValue())
        }));


        const robotTypeRepoStub = sinon.stub(robotTypeRepoInstance, "save").returns(new Promise<RobotType>((resolve, reject) => {
            resolve(RobotType.create({
                "id": req.body.id,
                "designacao": req.body.designacao,
                "tipoTarefas": req.body.tipoTarefas
            }).getValue())
        }));

        const service = new RobotTypeService(Container.get("RobotTypeRepo"));

        const robotTypeDTO = { "id": "123", "designacao": 'TypeX', "tipoTarefas": [TaskType.Task1] };

        // Act
        await service.updateRobotType(robotTypeDTO);

        // Assert
        sinon.assert.called(robotTypeStub);
        sinon.assert.calledOnce(robotTypeRepoStub);
        sinon.assert.calledWith(robotTypeStub, sinon.match(123));

    });
    */

});
